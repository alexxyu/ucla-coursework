`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    13:55:50 11/16/2021 
// Design Name: 
// Module Name:    state_machine 
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
module state_machine(
	clk, sw, btn, pmod_out, led
);

	input clk;
	input [7:0] sw;
	input [4:0] btn;
	
	output [3:0] pmod_out;
	output [2:0] led;

	parameter STATE_GROUND = 0;
	parameter STATE_PITCH_ADJUST = 1;
	parameter STATE_PLAY_PITCH = 2;
	
	reg [2:0] current_state = STATE_GROUND;
	reg [2:0] next_state = STATE_GROUND;
	assign led = current_state;

	reg [5:0] note_index = 0;	
	reg [5:0] next_note_index;
	
	wire play;
	assign play = current_state == STATE_PLAY_PITCH;
	
	always @* begin
		next_state = current_state;
		next_note_index = note_index;
		
		case (current_state)
			STATE_GROUND: begin
				if (btn[0]) begin
					next_state = STATE_PITCH_ADJUST;
				end 
			end
			STATE_PITCH_ADJUST: begin
				if (btn[1]) begin
					if (note_index == 0) begin
						next_note_index = 0;
					end else begin
						next_note_index = note_index - 6'b1;
					end
				end else if (btn[2]) begin
					if (note_index == 7) begin
						next_note_index = 7;
					end else begin
						next_note_index = note_index + 6'b1;
					end
				end else if (btn[0]) begin
					next_state = STATE_GROUND;
				end else if (btn[4]) begin
					next_state = STATE_PLAY_PITCH;
				end
			end
			STATE_PLAY_PITCH: begin
				if (btn[1]) begin
					if (note_index == 0) begin
						next_note_index = 0;
					end else begin
						next_note_index = note_index - 6'b1;
					end
				end else if (btn[2]) begin
					if (note_index == 7) begin
						next_note_index = 7;
					end else begin
						next_note_index = note_index + 6'b1;
					end
				end else if (btn[0]) begin
					next_state = STATE_GROUND;
				end else if (btn[4]) begin
					next_state = STATE_PITCH_ADJUST;
				end
			end
			default: begin
				next_state = current_state;
				next_note_index = note_index;
			end
		endcase
	end

	always @(posedge clk) begin
		current_state <= next_state;
		note_index <= next_note_index;
	end
	
	wire mclk, lrclk, sclk;
	
	clock clock_module(
		.main_clk(clk), .mclk(mclk), .lrclk(lrclk), .sclk(sclk)
	);
	
	wire [15:0] word;
	
	wave_generator gen(
		.clk(lrclk), .note_index(note_index), .word(word)
	);
	
	pmod_i2s_controller i2s_controller(
		.mclk(mclk), .lrclk(lrclk), .sclk(sclk), .word(word), .play(play), .pmod_out(pmod_out)
	);

endmodule
