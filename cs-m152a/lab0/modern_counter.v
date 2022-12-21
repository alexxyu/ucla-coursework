`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:45:33 09/28/2021 
// Design Name: 
// Module Name:    modern_counter 
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
module modern_counter(rst, clk, a
    );
	input rst, clk;
	output reg [3:0] a;

	always @ (posedge clk) begin
		if (rst) begin
			a <= 4'b0000;
		end else begin
			a <= a + 1'b1;
		end
	end

endmodule
