`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    13:55:36 11/16/2021 
// Design Name: 
// Module Name:    debouncer 
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
module debouncer(clk, sw_in, btn_in, sw_out, btn_out
    );

	input clk;
	input [7:0] sw_in;
	input [4:0] btn_in;
	
	output [7:0] sw_out;
	output [4:0] btn_out;
	
	switch_debounce sw0(
		.clk(clk), .signal(sw_in[0]), .output_signal(sw_out[0])	
	);
	
	switch_debounce sw1(
		.clk(clk), .signal(sw_in[1]), .output_signal(sw_out[1])	
	);
	
	switch_debounce sw2(
		.clk(clk), .signal(sw_in[2]), .output_signal(sw_out[2])	
	);
	
	switch_debounce sw3(
		.clk(clk), .signal(sw_in[3]), .output_signal(sw_out[3])	
	);
	
	switch_debounce sw4(
		.clk(clk), .signal(sw_in[4]), .output_signal(sw_out[4])	
	);
	
	switch_debounce sw5(
		.clk(clk), .signal(sw_in[5]), .output_signal(sw_out[5])	
	);
	
	switch_debounce sw6(
		.clk(clk), .signal(sw_in[6]), .output_signal(sw_out[6])	
	);
	
	switch_debounce sw7(
		.clk(clk), .signal(sw_in[7]), .output_signal(sw_out[7])	
	);
	
	debounce btn0(
		.clk(clk), .signal(btn_in[0]), .output_signal(btn_out[0])	
	);
	
	debounce btn1(
		.clk(clk), .signal(btn_in[1]), .output_signal(btn_out[1])	
	);
	
	debounce btn2(
		.clk(clk), .signal(btn_in[2]), .output_signal(btn_out[2])	
	);
	
	debounce btn3(
		.clk(clk), .signal(btn_in[3]), .output_signal(btn_out[3])	
	);
	
	debounce btn4(
		.clk(clk), .signal(btn_in[4]), .output_signal(btn_out[4])	
	);

endmodule
