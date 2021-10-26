`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:03:56 10/26/2021 
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
module top(clk, seg, an
    );
	
	input clk;
	output [6:0] seg;
	output [3:0] an;
	
	wire rst;
	wire [5:0] minutes;
	wire [5:0] seconds;
	
	assign rst = 0;

	wire hz1_clk, hz2_clk, faster_clk, blink_clk;
	
	clock clock_module(
		.main_clk(clk), .hz1_clk(hz1_clk), .hz2_clk(hz2_clk), .faster_clk(faster_clk), .blink_clk(blink_clk)
	);

	counter c(
		.clk(hz1_clk), .rst(rst), .minutes(minutes), .seconds(seconds)
	);
	
	display display_module(
		.clk(faster_clk), .minutes(minutes), .seconds(seconds), .segments(seg), .selects(an)
	);

endmodule
