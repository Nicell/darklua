use crate::nodes::{
    LuaxElement, LuaxFragment, LuaxChild, LuaxExpression, LuaxAttribute, LuaxOpeningElement, LuaxOpeningFragment, LuaxClosingElement, LuaxClosingFragment,
    Expression, FunctionCall, Arguments, Prefix, Identifier, TableExpression, TableFieldEntry, TableIndexEntry, TableEntry, FieldExpression, Variable,
};

use crate::process::{DefaultVisitor, Evaluator, NodeProcessor, NodeVisitor};
use crate::rules::{
    Context, FlawlessRule, RuleConfiguration, RuleConfigurationError, RuleProperties,
};

#[derive(Debug, Clone, Default)]
struct Converter {
    evaluator: Evaluator,
}

impl Converter {
    fn convert_luax_element_to_function_call(&self, element: &LuaxElement) -> Option<FunctionCall> {
        // Get name
        name = element.opening_element.name;

        // get attributes
        attributes = self.convert_luax_attributes_to_table_expression(element.opening_element.attributes);

        // get children
        children = self.convert_luax_children_to_table_expression(element.children);

        // Reconstruct Function Call
        FunctionCall::new(Prefix::new(Identifier::new("React")), Arguments::new(vec![name, attributes, children]), Identifier::new("createElement"))?;
    }

    fn convert_luax_fragment_to_function_call(&self, fragment: &LuaxFragment) -> Option<FunctionCall> {
        children = self.convert_luax_children_to_table_expression(fragment.children);

        FunctionCall::new(Prefix::new(Identifier::new("React")), Arguments::new(vec![FieldExpression::new(Prefix::new(Identifier::new("React")), Identifier::new("Fragment")), Expression::nil(), children]), Identifier::new("createElement"))?;
    }

    fn convert_luax_expression_to_expression(&self, expression: &LuaxExpression) -> Option<Expression> {
        expression.expression?;
    }

    fn convert_luax_attributes_to_table_expression(&self, attributes: &Vec<LuaxAttribute>) -> Option<TableExpression> {
        entries = attributes.iter().map(|attribute| {
            match attribute.name {
                Variable::Identifier(identifier) => TableFieldEntry::new(identifier, attribute.value),
                Variable::Field(field) => TableIndexEntry::new(field, attribute.value),
            }
        }).collect();

        TableExpression::new(entries)?;
    }

    fn convert_luax_children_to_table_expression(&self, children: &Vec<LuaxChild>) -> Option<TableExpression> {
        entries = children.iter().map(|child| {
            match child {
                LuaxChild::Element(element) => TableEntry::Value(self.convert_luax_element_to_function_call(element)),
                LuaxChild::Fragment(fragment) => TableEntry::Value(self.convert_luax_fragment_to_function_call(fragment)),
                LuaxChild::Expression(expression) => TableEntry::Value(self.convert_luax_expression_to_expression(expression)),
            }
        }).collect();

        TableExpression::new(entries)?;
    }
}

impl NodeProcessor for Converter {
    fn process_luax_element(&mut self, element: &mut LuaxElement) {
        mem::swap(element, &mut self.convert_luax_element_to_function_call(element).unwrap().into());
    }

    // fn process_luax_opening_element(&mut self, opening: &mut LuaxOpeningElement) {
        
    // }

    // fn process_luax_attributes(&mut self, table: &mut Vec<LuaxAttribute>) {
        
    // }

    // fn process_luax_attribute(&mut self, attribute: &mut LuaxAttribute) {
        
    // }

    // fn process_luax_closing_element(&mut self, variable: &mut LuaxClosingElement) {
        
    // }

    fn process_luax_fragment(&mut self, fragment: &mut LuaxFragment) {
        mem::swap(fragment, &mut self.convert_luax_fragment_to_function_call(fragment).unwrap().into());
    }

    // fn process_luax_opening_fragment(&mut self, opening: &mut LuaxOpeningFragment) {
        
    // }

    // fn process_luax_closing_fragment(&mut self, variable: &mut LuaxClosingFragment) {
        
    // }

    // fn process_luax_expression(&mut self, expression: &mut LuaxExpression) {
        
    // }

    // fn process_luax_children(&mut self, expression: &mut Vec<LuaxChild>) {
    //     // process luax children!

    //     // element
        
    //     // fragment

    //     // luax expression
    // }
}

pub const CONVERT_LUAX_TO_LUA_RULE_NAME: &str = "convert_luax_to_lua";

/// A rule that converts index expression into field expression.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ConvertLuaxToLua {}

impl FlawlessRule for ConvertLuaxToLua {
    fn flawless_process(&self, block: &mut Block, _: &Context) {
        let mut processor = Converter::default();
        DefaultVisitor::visit_block(block, &mut processor);
    }
}

impl RuleConfiguration for ConvertLuaxToLua {
    fn configure(&mut self, properties: RuleProperties) -> Result<(), RuleConfigurationError> {
        // Add rules for React vs. Roact and more?

        verify_no_rule_properties(&properties)?;

        Ok(())
    }

    fn get_name(&self) -> &'static str {
        CONVERT_LUAX_TO_LUA_RULE_NAME
    }

    fn serialize_to_properties(&self) -> RuleProperties {
        RuleProperties::new()
    }
}
