`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:26:39 10/28/2021 
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
module state_machine(clk, PAUSE, RESET, ADJ, SEL, adj_minutes, adj_seconds, paused_counting, paused_adj_seconds, paused_adj_minutes, state
    );

	input clk;
	
	input PAUSE, RESET, ADJ, SEL;

	output adj_minutes, adj_seconds, paused_counting, paused_adj_seconds, paused_adj_minutes;
    output [2:0] state;

	parameter STATE_COUNTING = 0;
	parameter STATE_ADJ_SECONDS = 1;
	parameter STATE_ADJ_MINUTES = 2;
	parameter STATE_PAUSED_COUNTING = 3;
	parameter STATE_PAUSED_ADJ_SECONDS = 4;
	parameter STATE_PAUSED_ADJ_MINUTES = 5;

	reg [2:0] current_state = STATE_COUNTING;
	reg [2:0] next_state = STATE_COUNTING;
	
    assign adj_minutes = current_state == STATE_ADJ_MINUTES;
    assign adj_seconds = current_state == STATE_ADJ_SECONDS;
	assign paused_counting = current_state == STATE_PAUSED_COUNTING;
	assign paused_adj_seconds = current_state == STATE_PAUSED_ADJ_SECONDS;
	assign paused_adj_minutes = current_state == STATE_PAUSED_ADJ_MINUTES;
	
    assign state = current_state;
  
    always @(posedge clk) begin
		current_state <= next_state;
	end
  
	always @* begin
        next_state = current_state;
		case (current_state)
			STATE_COUNTING: begin
				if (PAUSE == 1) begin
					next_state = STATE_PAUSED_COUNTING;
				end else if (ADJ == 1) begin
					if (SEL == 1) begin
						next_state = STATE_ADJ_SECONDS;
					end else begin
						next_state = STATE_ADJ_MINUTES;
					end
				end
			end
			STATE_ADJ_SECONDS: begin
				if (PAUSE == 1) begin
					next_state = STATE_PAUSED_ADJ_SECONDS;
				end else if (ADJ == 0) begin
					next_state = STATE_COUNTING;
				end else if (SEL == 0) begin
					next_state = STATE_ADJ_MINUTES;
				end
			end
			STATE_ADJ_MINUTES: begin
				if (PAUSE == 1) begin
					next_state = STATE_PAUSED_ADJ_MINUTES;
				end else if (ADJ == 0) begin
					next_state = STATE_COUNTING;
				end else if (SEL == 1) begin
					next_state = STATE_ADJ_SECONDS;
				end
			end
			STATE_PAUSED_COUNTING: begin
				if (PAUSE == 1) begin
					next_state = STATE_COUNTING;
				end else if (ADJ == 1) begin
					if (SEL == 1) begin
						next_state = STATE_PAUSED_ADJ_SECONDS;
					end else begin
						next_state = STATE_PAUSED_ADJ_MINUTES;
					end
				end
			end
			STATE_PAUSED_ADJ_SECONDS: begin
				if (PAUSE == 1) begin
					next_state = STATE_ADJ_SECONDS;
				end else if (ADJ == 0) begin
					next_state = STATE_PAUSED_COUNTING;
				end else if (SEL == 0) begin
					next_state = STATE_PAUSED_ADJ_MINUTES;
				end
			end
			STATE_PAUSED_ADJ_MINUTES: begin
				if (PAUSE == 1) begin
					next_state = STATE_ADJ_MINUTES;
				end else if (ADJ == 0) begin
					next_state = STATE_PAUSED_COUNTING;
				end else if (SEL == 1) begin
					next_state = STATE_ADJ_SECONDS;
				end
			end
			default: begin
				next_state = current_state;
			end
		endcase
	end

endmodule
