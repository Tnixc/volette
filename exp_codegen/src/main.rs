use cranelift::codegen::ir::{Function, UserExternalName, UserFuncName};
use cranelift::codegen::Context;
use cranelift::module::{default_libcall_names, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::isa::{self, CallConv};
use cranelift::prelude::settings::Flags;
use cranelift::prelude::types::{I32, I64};
use cranelift::prelude::{
    settings, AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, StackSlotData, StackSlotKind,
};
use target_lexicon::triple;

fn main() {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns.push(AbiParam::new(I64));
    sig.params.push(AbiParam::new(I32));
    let triple = triple!("arm64-apple-darwin");
    let flag_builder = settings::builder();
    let flags = Flags::new(flag_builder);
    let isa_builder = isa::lookup(triple).unwrap();
    let target_isa = isa_builder.finish(flags).unwrap();

    let builder = ObjectBuilder::new(target_isa, "name", default_libcall_names()).unwrap();
    let mut module = ObjectModule::new(builder);

    let mut ctx = Context::new();

    let mut fn_builder_ctx = FunctionBuilderContext::new();

    let func_name = UserFuncName::User(UserExternalName { namespace: 0, index: 0 });

    let func_id = module.declare_function("test", Linkage::Export, &sig).unwrap();
    let mut func = Function::with_name_signature(func_name, sig);
    let mut fn_builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
    let entry = fn_builder.create_block();
    fn_builder.switch_to_block(entry);
    fn_builder.append_block_params_for_function_params(entry);
    let val_x = fn_builder.block_params(entry)[0]; // param
    let stack_slot = fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 4, 0));

    // let y = fn_builder.declare_var(I32);
    // let tmp = fn_builder.ins().iconst(I32, 2);
    fn_builder.ins().stack_store(val_x, stack_slot, 0);
    // fn_builder.def_var(y, tmp);
    // let val_y = fn_builder.use_var(y);
    // fn_builder.def_var(y, tmp);
    // let val_ret = fn_builder.ins().iadd(val_x, val_y);
    let addr = fn_builder.ins().stack_addr(I64, stack_slot, 0);

    fn_builder.ins().return_(&[addr]);
    fn_builder.seal_block(entry);
    fn_builder.finalize();

    ctx.func = func;
    println!("{}", ctx.func.display());

    module.define_function(func_id, &mut ctx).unwrap();
    let product = module.finish();
    std::fs::write("test.o", product.emit().unwrap()).unwrap();
}
