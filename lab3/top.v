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
module top(clk, sw, btnl, btnr, seg, an
    );
	
	input clk;
	input [1:0] sw;
	input btnl, btnr;
	
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
	
	wire PAUSE;
	wire RESET;
	wire ADJ;
	wire SEL;
	
	debouncer debouncer(
		.clk(clk), .pause_btn(btnr), .rst_btn(btnl), .adj_sw(sw[0]), .sel_sw(sw[1]),
		.PAUSE(PAUSE), .RESET(RESET), .ADJ(ADJ), .SEL(SEL)
	);
	
	wire paused;

	state_machine fsm(
		.clk(hz1_clk), .PAUSE(PAUSE), .RESET(RESET), .ADJ(ADJ), .SEL(SEL), .paused(paused)
	);

	counter c(
		.clk(hz1_clk), .rst(rst), .paused(paused), .minutes(minutes), .seconds(seconds)
	);
	
	display display_module(
		.clk(faster_clk), .minutes(minutes), .seconds(seconds), .segments(seg), .selects(an)
	);

endmodule
