`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:45:13 10/14/2021 
// Design Name: 
// Module Name:    count_zeroes 
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
module count_zeroes(magnitude, exponent, significand, fifth_bit
    );
	
	input [11:0] magnitude;
	
	output reg [2:0] exponent;
	output reg [3:0] significand;
	output reg fifth_bit;
	
	integer test;
	integer found;
	
	always @* begin
		test = 11;
		found = 0;
		while (test > 3) begin
			if (!found && magnitude[test] == 1) begin
				if (test == 11) begin
					// Special case for -2048, which would have an exponent of 8, which is too big.
					exponent <= 7;
					significand <= 15;
					fifth_bit <= 1;
				end else begin
					exponent <= test - 3;
					significand <= magnitude >> (test - 3);
					fifth_bit <= magnitude >> (test - 4);
				end
				found = 1;
			end
			test = test - 1;
		end
		
		if (!found) begin
			exponent <= 0;
			significand <= magnitude;
			fifth_bit <= 0;
		end
	end


endmodule
