use std::mem;


use crate::nodes::{
    Block, Expression, FieldExpression, FunctionCall, Identifier, Prefix, Arguments, Variable, TableExpression, LuaxElement, LuaxFragment, LuaxExpression, LuaxAttribute, LuaxChild, TableEntry, TableIndexEntry, TableFieldEntry
};
use crate::process::{DefaultVisitor, NodeProcessor, NodeVisitor};
use crate::rules::{
    Context, FlawlessRule, RuleConfiguration, RuleConfigurationError, RuleProperties,
};

use super::verify_no_rule_properties;

#[derive(Debug, Default)]
struct LuaxConverter {}

// helper impls
impl LuaxConverter{ 
    fn convert_luax_element_to_function_call(&self, element: &LuaxElement) -> Option<FunctionCall>  {
        // Get name
        let name = element.opening_element.name;
        // get attributes
        let attributes = self.convert_luax_attributes_to_table_expression(&element.opening_element.attributes);
        // get children
        let children = self.convert_luax_children_to_table_expression(&element.children);
        // Reconstruct Function Call
        let arguments = Arguments::default();
        arguments.with_argument(name);
        arguments.with_argument(attributes);
        arguments.with_argument(children);

        let prefix = Prefix::from_name(Identifier::new("React"));

        let method = Some(Identifier::new("createElement"));

        
        return Some(FunctionCall::new(prefix, arguments, method));
    }

    fn convert_luax_fragment_to_function_call(&self, fragment: &LuaxFragment) -> Option<FunctionCall> {
        let children = self.convert_luax_children_to_table_expression(&fragment.children);
        
        let prefix = Prefix::from_name(Identifier::new("React"));

        let name = FieldExpression::new(Prefix::from_name(Identifier::new("React")), Identifier::new("Fragment"));
        
        let empty_attributes = Expression::nil();

        let arguments = Arguments::default();
        arguments.with_argument(name);
        arguments.with_argument(empty_attributes);
        arguments.with_argument(children);

        let method = Some(Identifier::new("createElement"));

        
        Some(FunctionCall::new(prefix, arguments, method))
    } 

    fn convert_luax_expression_to_expression(&self, expression: &LuaxExpression) -> Option<Expression> {
        Some(expression.expression)
    }

    fn convert_luax_attributes_to_table_expression(&self, attributes: &Vec<LuaxAttribute>) -> Option<TableExpression> {
        let entries = attributes.iter().map(|attribute: &LuaxAttribute| {
            match attribute.name {
                Variable::Identifier(identifier) => TableEntry::from(TableFieldEntry::new(identifier, attribute.value)),
                Variable::Field(field) => TableEntry::from(TableIndexEntry::new( Expression::Field(field), attribute.value)),
                Variable::Index(_) => todo!(),
            }
        }).collect();

        Some(TableExpression::new(entries))
    }

    fn convert_luax_children_to_table_expression(&self, children: &Vec<LuaxChild>) -> Option<TableExpression> {
        let entries = children.iter().map(|child| {
            match child {
                LuaxChild::Element(element) => TableEntry::Value(self.convert_luax_element_to_function_call(element).into()),
                LuaxChild::Fragment(fragment) => TableEntry::Value(self.convert_luax_fragment_to_function_call(fragment).into()),
                LuaxChild::Expression(expression) => TableEntry::Value(self.convert_luax_expression_to_expression(expression).into()),
            }
        }).collect();

        Some(TableExpression::new(entries))
    }

    // fn process_luax_element(&mut self, expression: &mut Expression) {
    //     // let function: Option<FunctionCall> = if let Expression::LuaxElement(element) = expression {
    //     //     self.convert_luax_element_to_function_call(element).map(Into::into)

    //     // } else {
    //     //     None
    //     // }
    //     // if let Expression::LuaxElement(element) = expression {
    //     //     self.convert_luax_element_to_function_call(&element).map(Into::into)
    //     // } else {
    //     //     None
    //     // };
    //     // if let Some(mut function) = function {
    //     //     mem::swap(element, &mut function);
    //     // }
    // }

    // fn process_luax_fragment(&mut self, fragment: &mut LuaxFragment) {
    //     //TODO process fragment
    //     // mem::swap(fragment, &mut self.convert_luax_fragment_to_function_call(fragment).unwrap().into());
    // }

    // fn process_luax_children(&mut self, expression: &mut Vec<LuaxChild>) {
    //     //TODO process children
    // }
}

impl NodeProcessor for LuaxConverter {
    fn process_expression(&mut self, expression: &mut Expression) {
        let function: Option<Expression> = if let Expression::LuaxElement(element) = expression {
            self.convert_luax_element_to_function_call(element).map(Into::into)
        } else if let Expression::LuaxFragment(element) = expression {
            self.convert_luax_fragment_to_function_call(element).map(Into::into)
        } else {
            None
        };
        if let Some(mut function) = function {
            mem::swap(expression, &mut function);
        }
    }
}

pub const LUAX_TO_LUA_RULE_NAME: &str = "luax to lua";

/// A rule that removes whitespaces associated with AST nodes.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct LuaxToLua {}

impl FlawlessRule for LuaxToLua {
    fn flawless_process(&self, block: &mut Block, _: &Context) {
        let mut processor = LuaxConverter::default();
        DefaultVisitor::visit_block(block, &mut processor);
    }
}

impl RuleConfiguration for LuaxToLua {
    fn configure(&mut self, properties: RuleProperties) -> Result<(), RuleConfigurationError> {
        verify_no_rule_properties(&properties)?;
        Ok(())
    }

    fn get_name(&self) -> &'static str {
        LUAX_TO_LUA_RULE_NAME
    }

    fn serialize_to_properties(&self) -> RuleProperties {
        RuleProperties::new()
    }
}


#[cfg(test)]
mod test {
    use super::*;

    use crate::rules::Rule;

    use insta::assert_json_snapshot;

    fn new_rule() -> LuaxToLua {
        LuaxToLua::default()
    }

    #[test]
    fn serialize_default_rule() {
        assert_json_snapshot!("convert_luax_to_lua", new_rule());
    }

    #[test]
    fn configure_with_extra_field_error() {
        let result = json5::from_str::<Box<dyn Rule>>(
            r#"{
            rule: 'convert_luax_to_lua',
            prop: "something",
        }"#,
        );
        pretty_assertions::assert_eq!(result.unwrap_err().to_string(), "unexpected field 'prop'");
    }
}
