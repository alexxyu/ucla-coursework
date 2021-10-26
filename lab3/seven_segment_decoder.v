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
module seven_segment_decoder(digit, segments
    );
	
	input [3:0] digit;
	output reg [6:0] segments;
	
	always @* begin
		case (digit)
			0: segments = ~'b0111111;
			1: segments = ~'b0000110;
			2: segments = ~'b1011011;
			3: segments = ~'b1001111;
			4: segments = ~'b1100110;
			5: segments = ~'b1101101;
			6: segments = ~'b1111101;
			7: segments = ~'b0000111;
			8: segments = ~'b1111111;
			9: segments = ~'b1101111;
		endcase
	end


endmodule
