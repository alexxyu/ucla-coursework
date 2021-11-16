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
module clock(main_clk, mclk, lrclk, sclk
    );
	
	input main_clk;
	
	output reg mclk = 1, lrclk = 1, sclk = 0;

	integer mclk_counter = 0;
	integer lrclk_counter = 0;
	integer sclk_counter = 0;

	always @(posedge main_clk) begin
		if (mclk_counter == 18 - 1) begin
			mclk = ~mclk;
			mclk_counter = 0;
		end else
			mclk_counter = mclk_counter + 1;
		
		if (lrclk_counter == 1152 - 1) begin
			lrclk = ~lrclk;
			lrclk_counter = 0;
		end else
			lrclk_counter = lrclk_counter + 1;
			
		if (sclk_counter == 36 - 1) begin
			sclk = ~sclk;
			sclk_counter = 0;
		end else
			sclk_counter = sclk_counter + 1;
	end

endmodule
