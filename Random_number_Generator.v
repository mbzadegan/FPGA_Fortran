// =============================================================
// xorshift64plus.v
// 64-bit Xorshift+ PRNG, suitable for FPGA
// =============================================================

module xorshift64plus #(
    parameter SEED1 = 64'h123456789ABCDEF0,
    parameter SEED2 = 64'hCAFEBABEDEADBEEF
)(
    input  wire        clk,
    input  wire        rst_n,      // active low reset
    input  wire        next,       // pulse: generate next random value
    output reg  [63:0] rand_out,
    output reg         valid
);

    reg [63:0] s0, s1, x;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            s0 <= SEED1;
            s1 <= SEED2;
            rand_out <= 0;
            valid <= 0;
        end else if (next) begin
            // Algorithm from Sebastiano Vigna, 2014
            x    <= s0;
            s0   <= s1;
            x    <= x ^ (x << 23);        // a
            s1   <= x ^ s1 ^ (x >> 17) ^ (s1 >> 26); // b,c
            rand_out <= s1 + s0;          // Output sum
            valid <= 1;
        end else begin
            valid <= 0;
        end
    end

endmodule
