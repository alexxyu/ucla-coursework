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
	parameter STATE_RHYTHM_ADJUST = 3;
	parameter STATE_PLAYBACK = 4;
	
	reg [2:0] current_state = STATE_GROUND;
	reg [2:0] next_state = STATE_GROUND;
	assign led = current_state;

	reg [3:0] note_index = 0;	
	reg [3:0] next_note_index;
	
	wire[2:0] sw_index;
	wire sw_valid;
	
	switch_demux sw_demux(.sw(sw), .select(sw_index), .valid(sw_valid));
	
	reg [3:0] pitches [7:0];
	reg [3:0] next_pitches [7:0];
	reg [2:0] pitch_select;
	reg [2:0] next_pitch_select;
	
	reg [3:0] rhythm [7:0];
	reg [3:0] next_rhythm [7:0];
	reg [2:0] rhythm_select;
	reg [2:0] next_rhythm_select;
	
	reg [2:0] playback_index = 0;

	wire play;
	assign play = current_state == STATE_PLAY_PITCH || current_state == STATE_PLAYBACK;
	
	initial begin
		pitches[0] = 0;
		pitches[1] = 0;
		pitches[2] = 0;
		pitches[3] = 0;
		pitches[4] = 0;
		pitches[5] = 0;
		pitches[6] = 0;
		pitches[7] = 0;
		
		rhythm[0] = 0;
		rhythm[1] = 0;
		rhythm[2] = 0;
		rhythm[3] = 0;
		rhythm[4] = 0;
		rhythm[5] = 0;
		rhythm[6] = 0;
		rhythm[7] = 0;
	end
	
	always @* begin
		next_state = current_state;
		next_note_index = note_index;
		next_pitch_select = pitch_select;
		next_rhythm_select = rhythm_select;
		
		next_pitches[0] = pitches[0];
		next_pitches[1] = pitches[1];
		next_pitches[2] = pitches[2];
		next_pitches[3] = pitches[3];
		next_pitches[4] = pitches[4];
		next_pitches[5] = pitches[5];
		next_pitches[6] = pitches[6];
		next_pitches[7] = pitches[7];
		
		next_rhythm[0] = rhythm[0];
		next_rhythm[1] = rhythm[1];
		next_rhythm[2] = rhythm[2];
		next_rhythm[3] = rhythm[3];
		next_rhythm[4] = rhythm[4];
		next_rhythm[5] = rhythm[5];
		next_rhythm[6] = rhythm[6];
		next_rhythm[7] = rhythm[7];
		
		case (current_state)
			STATE_GROUND: begin
				if (btn[0] && sw_valid) begin
					next_state = STATE_PITCH_ADJUST;
					next_pitch_select = sw_index;
					next_note_index = pitches[sw_index];
				end else if (btn[1] && sw_valid) begin
					next_state = STATE_RHYTHM_ADJUST;
					next_rhythm_select = sw_index;
				end else if (btn[4]) begin
					next_state = STATE_PLAYBACK;
				end
			end
			STATE_PITCH_ADJUST: begin
				if (btn[1]) begin
					if (note_index == 0) begin
						next_note_index = 0;
					end else begin
						next_note_index = note_index - 4'b1;
					end
				end else if (btn[2]) begin
					if (note_index == 12) begin
						next_note_index = 12;
					end else begin
						next_note_index = note_index + 4'b1;
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
						next_note_index = note_index - 4'b1;
					end
				end else if (btn[2]) begin
					if (note_index == 12) begin
						next_note_index = 12;
					end else begin
						next_note_index = note_index + 4'b1;
					end
				end else if (btn[0]) begin
					next_state = STATE_GROUND;
					next_pitches[pitch_select] = note_index;
				end else if (btn[4]) begin
					next_state = STATE_PITCH_ADJUST;
				end
			end
			STATE_RHYTHM_ADJUST: begin
				if (btn[1] && sw_valid) begin
					next_state = STATE_GROUND;
					next_rhythm[rhythm_select] = pitches[sw_index];
				end
			end
			STATE_PLAYBACK: begin
				if (btn[4]) begin
					next_state = STATE_GROUND;
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
		pitch_select <= next_pitch_select;
		rhythm_select <= next_rhythm_select;
		
		pitches[0] <= next_pitches[0];
		pitches[1] <= next_pitches[1];
		pitches[2] <= next_pitches[2];
		pitches[3] <= next_pitches[3];
		pitches[4] <= next_pitches[4];
		pitches[5] <= next_pitches[5];
		pitches[6] <= next_pitches[6];
		pitches[7] <= next_pitches[7];
		
		rhythm[0] <= next_rhythm[0];
		rhythm[1] <= next_rhythm[1];
		rhythm[2] <= next_rhythm[2];
		rhythm[3] <= next_rhythm[3];
		rhythm[4] <= next_rhythm[4];
		rhythm[5] <= next_rhythm[5];
		rhythm[6] <= next_rhythm[6];
		rhythm[7] <= next_rhythm[7];
		
		if (current_state == STATE_PLAYBACK) begin
			note_index <= rhythm[playback_index];
		end else begin
			note_index <= next_note_index;
		end
	end
	
	wire mclk, lrclk, sclk, rclk;
	
	clock clock_module(
		.main_clk(clk), .mclk(mclk), .lrclk(lrclk), .sclk(sclk), .rclk(rclk)
	);
	
	always @(posedge rclk) begin
		if (current_state == STATE_PLAYBACK) begin
			playback_index <= (playback_index + 1) % 8;
		end
	end
	
	wire [15:0] word;
	
	wave_generator gen(
		.clk(lrclk), .note_index(note_index), .word(word)
	);
	
	pmod_i2s_controller i2s_controller(
		.mclk(mclk), .lrclk(lrclk), .sclk(sclk), .word(word), .play(play), .pmod_out(pmod_out)
	);

endmodule
