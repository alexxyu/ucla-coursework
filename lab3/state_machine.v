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
module state_machine(clk, PAUSE, RESET, ADJ, SEL, paused, adj_minutes, adj_seconds, state
    );

	input clk;
	
	input PAUSE, RESET, ADJ, SEL;

	output paused, adj_minutes, adj_seconds;
   output [1:0] state;

	parameter STATE_COUNTING = 0;
	parameter STATE_PAUSED = 1;
	parameter STATE_ADJ_SECONDS = 2;
	parameter STATE_ADJ_MINUTES = 3;

	reg [1:0] current_state = STATE_COUNTING;
	reg [1:0] next_state = STATE_COUNTING;
	
	assign paused = current_state == STATE_PAUSED;
   assign adj_minutes = current_state == STATE_ADJ_MINUTES;
   assign adj_seconds = current_state == STATE_ADJ_SECONDS;
	
   assign state = current_state;
  
   always @(posedge clk) begin
		current_state <= next_state;
	end
  
	always @* begin
        next_state = current_state;
        if (RESET) begin
            next_state = STATE_COUNTING;
        end else begin
            case (current_state)
                STATE_COUNTING: begin
                    if (PAUSE == 1) begin
                        next_state = STATE_PAUSED;
                    end else if (ADJ == 1) begin
                        if (SEL == 1) begin
                            next_state = STATE_ADJ_SECONDS;
                        end else begin
                            next_state = STATE_ADJ_MINUTES;
                        end
                    end
                end
                STATE_PAUSED: begin
                    if (PAUSE == 1) begin
                        next_state = STATE_COUNTING;
                    end
                end
                STATE_ADJ_SECONDS: begin
                    if (ADJ == 0) begin
                        next_state = STATE_COUNTING;
                    end else if (SEL == 0) begin
                        next_state = STATE_ADJ_MINUTES;
                    end
                end
                STATE_ADJ_MINUTES: begin
                    if (ADJ == 0) begin
                        next_state = STATE_COUNTING;
                    end else if (SEL == 1) begin
                        next_state = STATE_ADJ_SECONDS;
                    end
                end
                default: begin
                    next_state = current_state;
                end
            endcase
        end
	end

endmodule
