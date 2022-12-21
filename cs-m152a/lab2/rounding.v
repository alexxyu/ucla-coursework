`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:31:52 10/14/2021 
// Design Name: 
// Module Name:    rounding 
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
module rounding(exponent, significand, fifth_bit, E, F
    );

	input [2:0] exponent;
	input [3:0] significand;
	input fifth_bit;
	
	output reg [2:0] E;
	output reg [3:0] F;
	
	always @* begin
		if (fifth_bit == 1) begin
			if (significand == 15) begin
				if (exponent != 7) begin
					E <= exponent + 1;
					F <= 8;
				end else begin
					E <= exponent;
					F <= significand;
				end
			end else begin
				E <= exponent;
				F <= significand + 1;
			end
		end else begin
			E <= exponent;
			F <= significand;
		end
	end


endmodule
