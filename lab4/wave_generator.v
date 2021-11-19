`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    13:58:19 11/16/2021 
// Design Name: 
// Module Name:    wave_generator 
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
module wave_generator(clk, note_index, word
    );
	
	input clk;
	input [5:0] note_index;
	
	output reg [15:0] word = 0;
	
	reg [15:0] note_table [6:0];
	
	initial begin
		note_table[0] = 166 - 1; // C3
		note_table[1] = 148 - 1; // D3
		note_table[2] = 132 - 1; // E3
		note_table[3] = 124 - 1; // F3
		note_table[4] = 111 - 1; // G3
		note_table[5] = 99 - 1;  // A3
		note_table[6] = 88 - 1;  // B3
	end
	
	integer counter = 0;
	
	always @(posedge clk) begin
		if (counter >= note_table[note_index]) begin
			counter <= 0;
			word <= ~word & 16'h0FFF;
		end else begin
			counter <= counter + 1;
		end
	end
endmodule
