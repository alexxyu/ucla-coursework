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
module count_zeroes(M, E, F, fifth_bit
    );
	
	input [11:0] M;
	
	output reg [2:0] E;
	output reg [3:0] F;
	output reg fifth_bit;
	
	integer test;
	integer found;
	
	always @* begin
		test = 11;
		found = 0;
		while (test > 3 && !found) begin
			if (M[test] == 1) begin
				if (test == 11) begin
					// Special case for -2048, which would have an exponent of 8, which is too big.
					E <= 7;
					F <= 15;
					fifth_bit <= 1;
				end else begin
					E <= test - 3;
					F <= M >> (test - 3);
					fifth_bit <= M >> (test - 4);
				end
				found = 1;
			end else begin
				test = test - 1;
			end
		end
		
		if (!found) begin
			E <= 0;
			F <= M;
			fifth_bit <= 0;
		end
	end


endmodule
