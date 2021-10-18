`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:35:30 10/14/2021 
// Design Name: 
// Module Name:    sign_magnitude 
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
module sign_magnitude(D, S, M
    );

	input [11:0] D;
	
	output reg S;
	output reg [11:0] M;
	
	always @* begin
		if (D[11] == 1) begin
			S <= 1;
			M <= ~D + 1;
		end else begin
			S <= 0;
			M <= D;
		end
	end

endmodule
