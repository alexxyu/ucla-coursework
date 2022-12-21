`timescale 1ns / 1ps

////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer:
//
// Create Date:   13:19:57 11/16/2021
// Design Name:   top
// Module Name:   E:/CS152A/lab4/tb.v
// Project Name:  lab4
// Target Device:  
// Tool versions:  
// Description: 
//
// Verilog Test Fixture created by ISE for module: top
//
// Dependencies:
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
////////////////////////////////////////////////////////////////////////////////

module tb;

	// Inputs
	reg clk;
	reg [7:0] sw;
	reg [4:0] btn;

	// Outputs
	wire [3:0] i2s;

	// Instantiate the Unit Under Test (UUT)
	top uut (
		.clk(clk), 
		.sw(sw), 
		.btn(btn), 
		.i2s(i2s)
	);

	initial begin
		// Initialize Inputs
		clk = 0;
		sw = 0;
		btn = 0;

		// Wait 100 ns for global reset to finish
		#100000;
		
		// Assign E3 to switch 0
		sw[0] = 1;
		#1000000;
		btn[0] = 1;
		#1000000;
		btn[0] = 0;
		#1000000;
		
		// Play pitch
		btn[4] = 1;
		#1000000;
		btn[4] = 0;
		#1000000;
		
		btn[2] = 1;
		#1000000;
		btn[2] = 0;
		#1000000;
		btn[2] = 1;
		#1000000;
		btn[2] = 0;
		#1000000;
		btn[2] = 1;
		#1000000;
		btn[2] = 0;
		#1000000;
		btn[2] = 1;
		#1000000;
		btn[2] = 0;
		#1000000;
		
		btn[0] = 1;
		#1000000;
		btn[0] = 0;
		#1000000;
		
		// Insert E3 as first note in measure
		btn[1] = 1;
		#1000000;
		btn[1] = 0;
		#1000000;
		
		btn[1] = 1;
		#1000000;
		btn[1] = 0;
		#1000000;
		
		// Insert C3 as second note in measure
		sw[0] = 0;
		sw[1] = 1;
		#1000000;
		btn[1] = 1;
		#1000000;
		btn[1] = 0;
		#1000000;
		sw[1] = 0;
		sw[4] = 1;
		btn[1] = 1;
		#1000000;
		btn[1] = 0;
		#1000000;
		
		// Playback
		btn[4] = 1;
		#1000000;
		btn[4] = 0;
		
		#100000000;
	end
	
	always begin
		#1;
		clk = ~clk;
	end
      
endmodule
