`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:08:41 10/26/2021 
// Design Name: 
// Module Name:    clock 
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
module clock(main_clk, hz1_clk, hz2_clk, faster_clk, blink_clk
    );
	
	input main_clk;
	
	output reg hz1_clk = 0, hz2_clk = 0, faster_clk = 0, blink_clk = 0;

	integer hz1_counter = 0;
	integer hz2_counter = 0;
	integer faster_counter = 0;
	integer blink_counter = 0;

	always @(posedge main_clk) begin
		if (hz1_counter == 50000000) begin
			hz1_clk <= ~hz1_clk;
			hz1_counter <= 0;
		end else
			hz1_counter <= hz1_counter + 1;
		
		if (hz2_counter == 25000000) begin
			hz2_clk <= ~hz2_clk;
			hz2_counter <= 0;
		end else
			hz2_counter <= hz2_counter + 1;
			
		if (faster_counter == 50000) begin
			faster_clk <= ~faster_clk;
			faster_counter <= 0;
		end else
			faster_counter <= faster_counter + 1;
			
		if (blink_counter == 12500000) begin
			blink_clk <= ~blink_clk;
			blink_counter <= 0;
		end else
			blink_counter <= blink_counter + 1;
	end

endmodule
