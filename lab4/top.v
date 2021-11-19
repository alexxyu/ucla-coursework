`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:        UCLA CS M152A
// Engineer:       Alex Yu, Cole Trammer
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
	output [3:0] i2s,
	output [2:0] led
);
	
	wire [7:0] sw_d;
	wire [4:0] btn_d;
	
	debouncer debouncer_module(
		.clk(clk), .sw_in(sw), .btn_in(btn), .sw_out(sw_d), .btn_out(btn_d)
	);
	
	state_machine sm(
		.clk(clk), .sw(sw_d), .btn(btn_d), .pmod_out(i2s), .led(led)
	);

endmodule
