`timescale 1ns / 1ns

////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer:
//
// Create Date:   15:54:19 10/26/2021
// Design Name:   top
// Module Name:   E:/CS152A/lab3/tb.v
// Project Name:  lab3
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
	reg clk = 0;
	
	reg [1:0] sw = 0;
	reg btns = 0, btnr = 0;

	// Outputs
	wire [6:0] seg;
	wire [3:0] an;

	// Instantiate the Unit Under Test (UUT)
	top uut (
		.clk(clk),
		.sw(sw),
		.btns(btns),
		.btnr(btnr),
		.seg(seg), 
		.an(an)
	);

	initial begin
		// Initialize Inputs
		clk = 0;

		// Wait 100 ns for global reset to finish
		#100;
        
		// Add stimulus here
		btns = 1;
		
		#10000000;
		
		$finish;
	end
	
	always begin
		#1 clk = ~clk;
	end
      
endmodule

