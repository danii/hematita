use self::super::{super::byte_code, BinaryOperation, OpCode, UnaryOperation};
use itertools::assert_equal;

#[test]
fn test_byte_code_macro() {
	let seventy_five = 75;
	let seventy_six = 76;
	let code = byte_code! {
		call 0, 1, 2;
		idxr 3, 4, 5;
		idxw 6, 7, 8;
		crt 9;
		aadd 10, 11, 12;
		asub 13, 14, 15;
		amul 16, 17, 18;
		adiv 19, 20, 21;
		afdiv 22, 23, 24;
		aexp 25, 26, 27;
		amod 28, 29, 30;
		band 31, 32, 33;
		bor 34, 35, 36;
		bxor 37, 38, 39;
		bshl 40, 41, 42;
		bshr 43, 44, 45;
		req 46, 47, 48;
		rne 49, 50, 51;
		rlt 52, 53, 54;
		rle 55, 56, 57;
		rgt 58, 59, 60;
		rge 61, 62, 63;
		cat 64, 65, 66;
		aneg 67, 68;
		bnot 69, 70;
		not 71, 72;
		len 73, 74;
		jmp seventy_five;
		cjmp seventy_six, 77;
		ret 78;
		reas 79, 80;
		lcst [81], 82;
		lglb {"eighty three"}, 84;
		sglb 85, {"eighty six"};
		luv ^87, 88;
		suv 89, ^90;
		noop;
	};

	assert_equal(code, vec![
		OpCode::Call {function: 0, arguments: 1, destination: 2},
		OpCode::IndexRead {indexee: 3, index: 4, destination: 5},
		OpCode::IndexWrite {indexee: 6, index: 7, value: 8},
		OpCode::Create {destination: 9},
		OpCode::BinaryOperation {operation: BinaryOperation::Add, left: 10, right: 11, destination: 12},
		OpCode::BinaryOperation {operation: BinaryOperation::Subtract, left: 13, right: 14, destination: 15},
		OpCode::BinaryOperation {operation: BinaryOperation::Multiply, left: 16, right: 17, destination: 18},
		OpCode::BinaryOperation {operation: BinaryOperation::Divide, left: 19, right: 20, destination: 21},
		OpCode::BinaryOperation {operation: BinaryOperation::FloorDivide, left: 22, right: 23, destination: 24},
		OpCode::BinaryOperation {operation: BinaryOperation::Exponent, left: 25, right: 26, destination: 27},
		OpCode::BinaryOperation {operation: BinaryOperation::Modulo, left: 28, right: 29, destination: 30},
		OpCode::BinaryOperation {operation: BinaryOperation::BitwiseAnd, left: 31, right: 32, destination: 33},
		OpCode::BinaryOperation {operation: BinaryOperation::BitwiseOr, left: 34, right: 35, destination: 36},
		OpCode::BinaryOperation {operation: BinaryOperation::BitwiseXOr, left: 37, right: 38, destination: 39},
		OpCode::BinaryOperation {operation: BinaryOperation::ShiftLeft, left: 40, right: 41, destination: 42},
		OpCode::BinaryOperation {operation: BinaryOperation::ShiftRight, left: 43, right: 44, destination: 45},
		OpCode::BinaryOperation {operation: BinaryOperation::Equal, left: 46, right: 47, destination: 48},
		OpCode::BinaryOperation {operation: BinaryOperation::NotEqual, left: 49, right: 50, destination: 51},
		OpCode::BinaryOperation {operation: BinaryOperation::LessThan, left: 52, right: 53, destination: 54},
		OpCode::BinaryOperation {operation: BinaryOperation::LessThanOrEqual, left: 55, right: 56, destination: 57},
		OpCode::BinaryOperation {operation: BinaryOperation::GreaterThan, left: 58, right: 59, destination: 60},
		OpCode::BinaryOperation {operation: BinaryOperation::GreaterThanOrEqual, left: 61, right: 62, destination: 63},
		OpCode::BinaryOperation {operation: BinaryOperation::Concat, left: 64, right: 65, destination: 66},
		OpCode::UnaryOperation {operation: UnaryOperation::Negate, operand: 67, destination: 68},
		OpCode::UnaryOperation {operation: UnaryOperation::BitwiseNot, operand: 69, destination: 70},
		OpCode::UnaryOperation {operation: UnaryOperation::LogicalNot, operand: 71, destination: 72},
		OpCode::UnaryOperation {operation: UnaryOperation::Length, operand: 73, destination: 74},
		OpCode::Jump {operation: 75, r#if: None},
		OpCode::Jump {operation: 76, r#if: Some(77)},
		OpCode::Return {result: 78},
		OpCode::ReAssign {actor: 79, destination: 80},
		OpCode::LoadConst {constant: 81, register: 82},
		OpCode::LoadGlobal {global: "eighty three", register: 84},
		OpCode::SaveGlobal {register: 85, global: "eighty six"},
		OpCode::LoadUpValue {up_value: 87, register: 88},
		OpCode::SaveUpValue {register: 89, up_value: 90},
		OpCode::NoOp
	])
}
