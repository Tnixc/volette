mod type_check;

use crate::compiler::parser::node::Node;

pub fn analysis_pass(root: &Node) {
    type_check::type_check(root);
}
