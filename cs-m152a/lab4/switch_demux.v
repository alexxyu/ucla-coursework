`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:37:22 11/23/2021 
// Design Name: 
// Module Name:    switch_demux 
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
module switch_demux(
		input [7:0] sw, output reg [2:0] select, output reg valid
    );
	
	integer sw_index = 0;
	reg found_one_switch = 0;
	
	always @* begin
		sw_index = 0;
		found_one_switch = 0;
		valid = 0;
		select = 0;
		
		while (sw_index < 8) begin
			if (sw[sw_index]) begin
				if (found_one_switch) begin
					valid = 0;
				end else begin
					select = sw_index;
					valid = 1;
					found_one_switch = 1;
				end
			end
			sw_index = sw_index + 1;
		end
	end


endmodule
