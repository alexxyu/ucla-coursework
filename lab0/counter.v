`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    09:32:57 09/24/2021 
// Design Name: 
// Module Name:    counter 
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
module counter(rst, clk, a0, a1, a2, a3
    );
	input rst, clk;
	output reg a0, a1, a2, a3;

	always @ (posedge clk) begin
		if (rst) begin
			a0 <= 1'b0;
			a1 <= 1'b0;
			a2 <= 1'b0;
			a3 <= 1'b0;
		end else begin
			a0 <= ~a0;
			a1 <= a1 ^ a0;
			a2 <= a2 ^ (a1 & a0);
			a3 <= a3 ^ (a2 & a1 & a0);
		end
	end

endmodule
