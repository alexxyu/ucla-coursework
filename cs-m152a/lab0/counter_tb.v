`timescale 1ns / 1ps

////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer:
//
// Create Date:   13:00:49 09/23/2021
// Design Name:   mux
// Module Name:   E:/CS 152A/lab0/mux_tb0.v
// Project Name:  lab0
// Target Device:  
// Tool versions:  
// Description: 
//
// Verilog Test Fixture created by ISE for module: mux
//
// Dependencies:
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
////////////////////////////////////////////////////////////////////////////////

module counter_tb0;

	// Inputs
	reg rst;
	reg clk;

	// Outputs
	wire a0, a1, a2, a3;

	// Instantiate the Unit Under Test (UUT)
	counter uut (
		.rst(rst),
		.clk(clk),
		.a0(a0),
		.a1(a1),
		.a2(a2),
		.a3(a3)
	);

	initial begin
		// Initialize Inputs
		clk = 0;
		rst = 0;
		// Wait 100 ns for global reset to finish
		#100;
		// Add stimulus here
		rst = 1;
		#20 rst = 0;

		// add verification routine here
		#1000 $finish;
	end
	
	always begin
		#20 clk = ~clk;
	end
	
endmodule

