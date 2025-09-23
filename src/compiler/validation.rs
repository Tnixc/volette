use generational_arena::Arena;
use std::collections::{HashMap, HashSet};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    error::{CompilerPhase, Diagnostic, DiagnosticCollection, Severity},
    parser::node::{DefKind, ExprKind, Node, NodeKind, Type},
    tokens::DisplaySpan,
};

/// Validates the AST for structural correctness and semantic issues before codegen
pub fn validate_ast(
    root: &Node,
    nodes: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
) -> Result<(), DiagnosticCollection> {
    let mut validator = AstValidator::new(nodes, interner);
    validator.validate_root(root);

    if validator.diagnostics.has_errors() {
        Err(validator.diagnostics)
    } else {
        Ok(())
    }
}

struct AstValidator<'a> {
    nodes: &'a Arena<Node>,
    interner: &'a StringInterner<BucketBackend<SymbolUsize>>,
    diagnostics: DiagnosticCollection,
    declared_functions: HashMap<SymbolUsize, FunctionSignature>,
    current_function: Option<SymbolUsize>,
    in_loop: bool,
}

#[derive(Debug, Clone)]
struct FunctionSignature {
    name: SymbolUsize,
    params: Vec<(SymbolUsize, Type)>,
    return_type: Type,
    span: DisplaySpan,
}

impl<'a> AstValidator<'a> {
    fn new(nodes: &'a Arena<Node>, interner: &'a StringInterner<BucketBackend<SymbolUsize>>) -> Self {
        Self {
            nodes,
            interner,
            diagnostics: DiagnosticCollection::new(),
            declared_functions: HashMap::new(),
            current_function: None,
            in_loop: false,
        }
    }

    fn validate_root(&mut self, root: &Node) {
        match &root.kind {
            NodeKind::Root { defs } => {
                // first pass: collect all function declarations
                for &def_idx in defs {
                    if let Some(def_node) = self.nodes.get(def_idx) {
                        self.collect_function_declaration(def_node);
                    }
                }

                // second pass: validate function bodies
                for &def_idx in defs {
                    if let Some(def_node) = self.nodes.get(def_idx) {
                        self.validate_definition(def_node);
                    }
                }

                // check for unused functions (warnings)
                self.check_unused_functions();
            }
            _ => {
                self.add_error("Expected root node at top level".to_string(), root.span.to_display(self.interner));
            }
        }
    }

    fn collect_function_declaration(&mut self, node: &Node) {
        if let NodeKind::Def {
            kind: DefKind::Function {
                name, params, return_type, ..
            },
        } = &node.kind
        {
            let signature = FunctionSignature {
                name: *name,
                params: params.iter().map(|(name, ty, _)| (*name, *ty)).collect(),
                return_type: *return_type,
                span: node.span.to_display(self.interner),
            };

            if let Some(existing) = self.declared_functions.get(name) {
                let existing_span = existing.span.clone();
                self.add_error(
                    format!(
                        "Function '{}' is already declared",
                        self.interner.resolve(*name).unwrap_or("<unknown>")
                    ),
                    node.span.to_display(self.interner),
                );
                self.add_note(format!("Previous declaration was here"), existing_span);
            } else {
                self.declared_functions.insert(*name, signature);
            }
        }
    }

    fn validate_definition(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Def { kind } => match kind {
                DefKind::Function {
                    name,
                    params,
                    body,
                    return_type,
                } => {
                    self.current_function = Some(*name);

                    // Validate parameter names are unique
                    let mut param_names = HashSet::new();
                    for (param_name, _, param_span) in params {
                        if param_names.contains(param_name) {
                            self.add_error(
                                format!(
                                    "Parameter '{}' is declared multiple times",
                                    self.interner.resolve(*param_name).unwrap_or("<unknown>")
                                ),
                                param_span.to_display(self.interner),
                            );
                        } else {
                            param_names.insert(*param_name);
                        }
                    }

                    // Validate function body
                    if let Some(body_node) = self.nodes.get(*body) {
                        self.validate_expression(body_node, Some(*return_type));
                    }

                    self.current_function = None;
                }
                DefKind::Struct { name: _, fields } => {
                    // Validate struct fields are unique
                    let mut field_names = HashSet::new();
                    for (field_name, _, field_span) in fields {
                        if field_names.contains(field_name) {
                            self.add_error(
                                format!(
                                    "Field '{}' is declared multiple times",
                                    self.interner.resolve(*field_name).unwrap_or("<unknown>")
                                ),
                                field_span.to_display(self.interner),
                            );
                        } else {
                            field_names.insert(*field_name);
                        }
                    }
                }
            },
            _ => {
                self.add_error("Expected definition node".to_string(), node.span.to_display(self.interner));
            }
        }
    }

    fn validate_expression(&mut self, node: &Node, expected_type: Option<Type>) {
        match &node.kind {
            NodeKind::Expr { kind, type_: _ } => {
                match kind {
                    ExprKind::Literal(_) => {
                        // Literals are always valid
                    }
                    ExprKind::Identifier(symbol) => {
                        // Check if identifier is declared (this would be caught in analysis, but good to double-check)
                        let name = self.interner.resolve(*symbol).unwrap_or("<unknown>");
                        if !self.is_identifier_in_scope(*symbol) {
                            self.add_warning(
                                format!("Identifier '{}' may not be in scope", name),
                                node.span.to_display(self.interner),
                            );
                        }
                    }
                    ExprKind::BinOp { left, right, op: _ } => {
                        if let Some(left_node) = self.nodes.get(*left) {
                            self.validate_expression(left_node, None);
                        }
                        if let Some(right_node) = self.nodes.get(*right) {
                            self.validate_expression(right_node, None);
                        }
                    }
                    ExprKind::Call { func, args } => {
                        // Validate function exists
                        if let Some(func_node) = self.nodes.get(*func) {
                            if let NodeKind::Expr {
                                kind: ExprKind::Identifier(func_name),
                                ..
                            } = &func_node.kind
                            {
                                if let Some(signature) = self.declared_functions.get(func_name) {
                                    // Check argument count
                                    if args.len() != signature.params.len() {
                                        self.add_error(
                                            format!(
                                                "Function '{}' expects {} arguments, but {} were provided",
                                                self.interner.resolve(*func_name).unwrap_or("<unknown>"),
                                                signature.params.len(),
                                                args.len()
                                            ),
                                            node.span.to_display(self.interner),
                                        );
                                    }
                                } else {
                                    self.add_error(
                                        format!("Undefined function '{}'", self.interner.resolve(*func_name).unwrap_or("<unknown>")),
                                        func_node.span.to_display(self.interner),
                                    );
                                }
                            }
                        }

                        // Validate arguments
                        for &arg_idx in args {
                            if let Some(arg_node) = self.nodes.get(arg_idx) {
                                self.validate_expression(arg_node, None);
                            }
                        }
                    }
                    ExprKind::Block { exprs } => {
                        for &expr_idx in exprs {
                            if let Some(expr_node) = self.nodes.get(expr_idx) {
                                self.validate_expression(expr_node, None);
                            }
                        }
                    }
                    ExprKind::Return { value } => {
                        if self.current_function.is_none() {
                            self.add_error(
                                "Return statement outside of function".to_string(),
                                node.span.to_display(self.interner),
                            );
                        }

                        if let Some(value_idx) = value {
                            if let Some(value_node) = self.nodes.get(*value_idx) {
                                self.validate_expression(value_node, expected_type);
                            }
                        }
                    }
                    ExprKind::Break => {
                        if !self.in_loop {
                            self.add_error("Break statement outside of loop".to_string(), node.span.to_display(self.interner));
                        }
                    }
                    ExprKind::Loop { body } => {
                        let was_in_loop = self.in_loop;
                        self.in_loop = true;

                        if let Some(body_node) = self.nodes.get(*body) {
                            self.validate_expression(body_node, None);
                        }

                        self.in_loop = was_in_loop;
                    }
                    ExprKind::LetBinding { name: _, value, .. } => {
                        if let Some(value_node) = self.nodes.get(*value) {
                            self.validate_expression(value_node, None);
                        }
                    }
                    ExprKind::Assign { target, value } => {
                        // Validate assignment target is an lvalue
                        if let Some(target_node) = self.nodes.get(*target) {
                            match &target_node.kind {
                                NodeKind::Expr {
                                    kind: ExprKind::Identifier(_),
                                    ..
                                } => {
                                    // Valid lvalue
                                }
                                _ => {
                                    self.add_error("Invalid assignment target".to_string(), target_node.span.to_display(self.interner));
                                }
                            }
                        }

                        if let Some(value_node) = self.nodes.get(*value) {
                            self.validate_expression(value_node, None);
                        }
                    }
                    ExprKind::If {
                        cond,
                        then_block,
                        else_block,
                    } => {
                        if let Some(cond_node) = self.nodes.get(*cond) {
                            self.validate_expression(cond_node, None);
                        }
                        if let Some(then_node) = self.nodes.get(*then_block) {
                            self.validate_expression(then_node, None);
                        }
                        if let Some(else_idx) = else_block {
                            if let Some(else_node) = self.nodes.get(*else_idx) {
                                self.validate_expression(else_node, None);
                            }
                        }
                    }
                    ExprKind::UnaryOp { expr, .. } => {
                        if let Some(expr_node) = self.nodes.get(*expr) {
                            self.validate_expression(expr_node, None);
                        }
                    }
                    ExprKind::Cast { expr, target_type: _ } => {
                        if let Some(expr_node) = self.nodes.get(*expr) {
                            self.validate_expression(expr_node, None);
                        }
                        // TODO: Validate cast is legal
                    }
                    ExprKind::ParenExpr { expr } => {
                        if let Some(expr_node) = self.nodes.get(*expr) {
                            self.validate_expression(expr_node, expected_type);
                        }
                    }
                    ExprKind::BlockReturn { value } => {
                        if let Some(value_idx) = value {
                            if let Some(value_node) = self.nodes.get(*value_idx) {
                                self.validate_expression(value_node, expected_type);
                            }
                        }
                    }
                }
            }
            _ => {
                self.add_error("Expected expression node".to_string(), node.span.to_display(self.interner));
            }
        }
    }

    fn is_identifier_in_scope(&self, _symbol: SymbolUsize) -> bool {
        // TODO: rn we assume it's valid since the analysis phase should catch this.
        true
        // ???
    }

    fn check_unused_functions(&mut self) {
        // for now, just check if main exists
        let main_symbol = self.interner.get("main");
        if let Some(main_sym) = main_symbol {
            if !self.declared_functions.contains_key(&main_sym) {
                self.add_warning(
                    "No 'main' function found. Program may not be executable.".to_string(),
                    DisplaySpan {
                        file: "<unknown>".to_string(),
                        start: (1, 1),
                        end: (1, 1),
                    },
                );
            }
        }
        // TODO: reachability analysis
    }

    fn add_error(&mut self, message: String, span: DisplaySpan) {
        self.diagnostics
            .push(Diagnostic::error(message, CompilerPhase::Validation).with_span(span));
    }

    fn add_warning(&mut self, message: String, span: DisplaySpan) {
        self.diagnostics
            .push(Diagnostic::warning(message, CompilerPhase::Validation).with_span(span));
    }

    fn add_note(&mut self, message: String, span: DisplaySpan) {
        self.diagnostics
            .push(Diagnostic::new(Severity::Info, message, CompilerPhase::Validation).with_span(span));
    }
}
