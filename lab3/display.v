`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:16:28 10/26/2021 
// Design Name: 
// Module Name:    display 
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
module display(clk, blink_clk, adj_minutes, adj_seconds, minutes, seconds, segments, selects
    );
	
	input clk, blink_clk, adj_minutes, adj_seconds;
	input [5:0] minutes;
	input [5:0] seconds;
	
	output reg [6:0] segments;
	output reg [3:0] selects;

   reg blink_state = 0;

	integer display_number = 0;
	
	wire [3:0] minutes_tens;
	wire [3:0] minutes_ones;
	
	wire [3:0] seconds_tens;
	wire [3:0] seconds_ones;
	
	assign minutes_tens = minutes / 10;
	assign minutes_ones = minutes % 10;
	
	assign seconds_tens = seconds / 10;
	assign seconds_ones = seconds % 10;
	
   wire clear_minutes;
   wire clear_seconds;
    
   assign clear_minutes = (adj_minutes && blink_state);
   assign clear_seconds = (adj_seconds && blink_state);
    
	wire [6:0] minutes_tens_segments;
	wire [6:0] minutes_ones_segments;
	wire [6:0] seconds_tens_segments;
	wire [6:0] seconds_ones_segments;

	seven_segment_decoder mt(
		.digit(minutes_tens), .force_clear(clear_minutes), .segments(minutes_tens_segments)
	);
	
	seven_segment_decoder mo(
		.digit(minutes_ones), .force_clear(clear_minutes), .segments(minutes_ones_segments)
	);
	
	seven_segment_decoder st(
		.digit(seconds_tens), .force_clear(clear_seconds), .segments(seconds_tens_segments)
	);
	
	seven_segment_decoder so(
		.digit(seconds_ones), .force_clear(clear_seconds), .segments(seconds_ones_segments)
	);

   always @(posedge blink_clk) begin
		if (adj_minutes || adj_seconds) begin
			blink_state <= !blink_state;
		end else begin
			blink_state <= 0;
		end
   end

	always @(posedge clk) begin
		case (display_number)
			0: begin
				segments <= minutes_tens_segments;
				selects <= 'b0111;
			   end
			1: begin
				segments <= minutes_ones_segments;
			   selects <= 'b1011;
			   end
			2: begin
				segments <= seconds_tens_segments;
			   selects <= 'b1101;
			   end
			3: begin
				segments <= seconds_ones_segments;
			   selects <= 'b1110;
			   end
		endcase
		display_number <= (display_number + 1'b1) % 4;
	end

endmodule
