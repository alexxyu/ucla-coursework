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
	input [4:0] note_index;
	
	output reg [15:0] word = 0;
	
	reg [15:0] note_table [16:0];
	
    // Value is set to 43.4 KHz / (frequency of note) / 2
	initial begin
		note_table[0] = 166 - 1; // C3
		note_table[1] = 157 - 1; // C#3
		note_table[2] = 148 - 1; // D3
		note_table[3] = 140 - 1; // D#3
		note_table[4] = 132 - 1; // E3
		note_table[5] = 124 - 1; // F3
		note_table[6] = 117 - 1; // F#3
		note_table[7] = 111 - 1; // G3
		note_table[8] = 105 - 1; // G#3
		note_table[9] = 99 - 1;  // A3
		note_table[10] = 93 - 1; // A#3
		note_table[11] = 88 - 1; // B3
		note_table[12] = 83 - 1; // C4
		note_table[13] = 78 - 1; // C#4
		note_table[14] = 74 - 1; // D4
		note_table[15] = 70 - 1; // D#4
		note_table[16] = 66 - 1; // E4
	end
	
	integer counter = 0;
	
	always @(posedge clk) begin
		if (note_index == -1) begin
			counter <= 0;
			word <= 0;
		end else if (counter >= note_table[note_index]) begin
			counter <= 0;
			word <= ~word & 16'h3FFF;
		end else begin
			counter <= counter + 1;
		end
	end
endmodule
