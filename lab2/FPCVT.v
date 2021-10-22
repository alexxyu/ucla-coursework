`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:36:38 10/14/2021 
// Design Name: 
// Module Name:    FPCVT 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module FPCVT(D, S, E, F
    );
	input [11:0] D;

	output wire S;
	output wire [2:0] E;
	output wire [3:0] F;

	wire [11:0] magnitude;

	sign_magnitude sm(
		.D(D), .S(S), .magnitude(magnitude)
	);

	wire [2:0] exponent;
	wire [3:0] significand;
	wire fifth_bit;

	count_zeroes cz(
		.magnitude(magnitude), .exponent(exponent), .significand(significand), .fifth_bit(fifth_bit)
	);

	rounding r(
		.exponent(exponent), .significand(significand), .fifth_bit(fifth_bit), .E(E), .F(F)
	);

endmodule
