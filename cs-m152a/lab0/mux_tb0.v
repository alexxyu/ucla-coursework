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

module mux_tb0;

	// Inputs
	reg [4:0] sw;

	// Outputs
	wire led;

	// Instantiate the Unit Under Test (UUT)
	mux uut (
		.sw(sw), 
		.led(led)
	);

	initial begin
		// Initialize Inputs
		sw = 5'b11110;

		// Wait 100 ns for global reset to finish
		#100;
        
		// Add stimulus here	
		sw = 5'b11110;
		
		
	end
	
endmodule

