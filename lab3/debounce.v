`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:52:51 10/28/2021 
// Design Name: 
// Module Name:    debounce 
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
module debounce(clk, signal, output_signal
    );
	
	input clk, signal;
	output reg output_signal;
	
	reg [16:0] clk_dv = 0;
	reg [2:0] step_d = 0;
	
	reg clk_en = 0;
	reg clk_en_d = 0;
	
	wire [17:0] clk_dv_inc;
	assign clk_dv_inc = clk_dv + 1;

	always @(posedge clk) begin
		clk_dv <= clk_dv_inc[16:0];
		clk_en <= clk_dv_inc[17];
		clk_en_d <= clk_en;
	end
	
	always @(posedge clk) begin
		if (clk_en) begin
			step_d[2:0] <= { signal, step_d[2:1] };
		end
	end

	always @(posedge clk) begin
		output_signal <= ~step_d[0] & step_d[1] & clk_en_d;
	end

endmodule
