`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:22:45 10/26/2021 
// Design Name: 
// Module Name:    seven_segment_decoder 
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
module seven_segment_decoder(digit, force_clear, segments
    );
	
	input [3:0] digit;
   input force_clear;
	output reg [6:0] segments;
	
	always @* begin
        if (force_clear) begin
            segments = 7'b1111111;
        end else begin
            case (digit)
                0: segments = ~7'b0111111;
                1: segments = ~7'b0000110;
                2: segments = ~7'b1011011;
                3: segments = ~7'b1001111;
                4: segments = ~7'b1100110;
                5: segments = ~7'b1101101;
                6: segments = ~7'b1111101;
                7: segments = ~7'b0000111;
                8: segments = ~7'b1111111;
                9: segments = ~7'b1101111;
					 default: segments = 7'b1111111;
            endcase
        end
	end

endmodule
