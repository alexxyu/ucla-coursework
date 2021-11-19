`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:38:17 11/16/2021 
// Design Name: 
// Module Name:    pmod_i2s_controller 
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
module pmod_i2s_controller(mclk, lrclk, sclk, word, play, pmod_out);
	input mclk, lrclk, sclk, play;
	input [15:0] word;
	
	output wire [3:0] pmod_out;

	assign pmod_out = { serial_out, sclk, lrclk, mclk };

	reg [15:0] current_word;
	reg [15:0] next_word;

	integer bit_index = 0;
	reg serial_out = 0;

	always @(posedge lrclk) begin
		next_word = word;
	end
	
	always @(negedge sclk) begin
		serial_out = current_word[bit_index] & play;

		if (bit_index == 0) begin
			current_word <= next_word;
			bit_index <= 15;
		end else begin
			bit_index <= bit_index - 1;
		end
	end

endmodule
