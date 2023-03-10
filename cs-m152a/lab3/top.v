`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:        UCLA CS M152A
// Engineer:       Alex Yu and Nicolas Trammer
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
module top(clk, sw, btnl, btnr, seg, an, Led
    );
	
	input clk;
	input [1:0] sw;
	input btnl, btnr;
	
	output [6:0] seg;
	output [3:0] an;
    output [2:0] Led;

	wire [5:0] minutes;
	wire [5:0] seconds;

	wire hz1_clk, hz2_clk, faster_clk, blink_clk;
	
	clock clock_module(
		.main_clk	(clk), 
		.hz1_clk		(hz1_clk), 
		.hz2_clk		(hz2_clk), 
		.faster_clk	(faster_clk), 
		.blink_clk	(blink_clk)
	);
	
	wire PAUSE;
	wire RESET;
	wire ADJ;
	wire SEL;
    
    wire [2:0] state;
    assign Led = state;
	
	debouncer debouncer(
		.clk			(clk), 
		.pause_btn	(btnl), 
		.rst_btn		(btnr), 
		.adj_sw		(sw[0]), 
		.sel_sw		(sw[1]),
		.PAUSE		(PAUSE), 
		.RESET		(RESET), 
		.ADJ			(ADJ), 
		.SEL			(SEL)
	);
	
	wire adj_minutes, adj_seconds, paused_counting, paused_adj_seconds, paused_adj_minutes;

	state_machine fsm(
		.clk				(clk), 
		.PAUSE				(PAUSE), 
		.RESET				(RESET), 
		.ADJ				(ADJ), 
		.SEL				(SEL), 
		.adj_minutes		(adj_minutes), 
		.adj_seconds 		(adj_seconds),
		.paused_counting    (paused_counting), 
		.paused_adj_seconds (paused_adj_seconds), 
		.paused_adj_minutes (paused_adj_minutes),
		.state				(state)
	);

   wire counter_clk;
   assign counter_clk = (adj_minutes || adj_seconds) ? hz2_clk: hz1_clk;
   assign paused = paused_counting || paused_adj_seconds || paused_adj_minutes;

	counter c(
		.clk			(counter_clk), 
		.rst			(RESET), 
		.paused		(paused), 
		.adj_minutes(adj_minutes), 
		.adj_seconds(adj_seconds), 
		.minutes		(minutes), 
		.seconds		(seconds)
	);
	
	assign display_adj_minutes = (adj_minutes || paused_adj_minutes);
	assign display_adj_seconds = (adj_seconds || paused_adj_seconds);

	display display_module(
		.clk			(faster_clk), 
		.blink_clk	(blink_clk), 
		.adj_minutes(display_adj_minutes), 
		.adj_seconds(display_adj_seconds), 
		.minutes		(minutes), 
		.seconds		(seconds), 
		.segments	(seg), 
		.selects		(an)
	);

endmodule
