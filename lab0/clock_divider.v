`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:00:10 09/28/2021 
// Design Name: 
// Module Name:    clock_divider 
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
module clock_divider(clk, led
    );
	input clk;
	output reg led;

	reg [31:0] counter;

	initial begin
		led = 1;
		counter = 32'b0;
	end

	always @ (posedge clk) begin
		if (counter == 50000000) begin
			counter <= 0;
			led <= ~led;
		end else begin
			counter <= counter + 1'b1;
		end
	end

endmodule
