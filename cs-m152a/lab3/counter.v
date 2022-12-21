`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:04:22 10/26/2021 
// Design Name: 
// Module Name:    counter 
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
module counter(clk, rst, paused, adj_minutes, adj_seconds, minutes, seconds
    );
	
	input clk, rst, paused, adj_minutes, adj_seconds;
	
	output reg [5:0] minutes = 0;
	output reg [5:0] seconds = 0;

	always @(posedge clk or posedge rst) begin
        if (rst) begin
            minutes <= 0;
            seconds <= 0;
        end else if (paused) begin
			/* Do nothing */
		end else if (adj_minutes) begin
            minutes <= (minutes + 1'b1) % 60;
        end else if (adj_seconds) begin
            seconds <= (seconds + 1'b1) % 60;
        end else begin
			if (seconds < 59) begin
				seconds <= seconds + 1'b1;
			end else begin
				seconds <= 0;
				if (minutes < 59) begin
					minutes <= minutes + 1'b1;
				end else begin
					minutes <= 0;
				end
			end
		end
	end

endmodule
