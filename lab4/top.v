`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    13:36:55 11/09/2021 
// Design Name: 
// Module Name:    top 
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
module top(
	input clk,
	input [7:0] sw,
	input [4:0] btn,
	output [3:0] i2s
);
	
	wire mclk, lrclk, sclk;
	
	clock clock_module(
		.main_clk(clk), .mclk(mclk), .lrclk(lrclk), .sclk(sclk)
	);
	
	reg [15:0] word = 0;
	integer counter = 0;
	
	pmod_i2s_controller i2s_controller(
		.mclk(mclk), .lrclk(lrclk), .sclk(sclk), .word(word), .pmod_out(i2s)
	);
	
	always @(posedge clk) begin
		if (counter == 113636) begin
			counter <= 0;
			word <= ~word;
		end else begin
			counter <= counter + 1;
		end
	end

endmodule
