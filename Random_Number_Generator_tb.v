`timescale 1ns/1ps
module xorshift64plus_tb;

    reg clk = 0;
    always #5 clk = ~clk;

    reg rst_n = 0;
    reg next = 0;
    wire [63:0] rand_out;
    wire valid;

    xorshift64plus dut (
        .clk(clk), .rst_n(rst_n), .next(next),
        .rand_out(rand_out), .valid(valid)
    );

    integer i;
    initial begin
        $display("Starting Xorshift64+ PRNG test...");
        #10 rst_n = 1;
        #10;
        for (i = 0; i < 10; i = i + 1) begin
            @(posedge clk);
            next = 1;
            @(posedge clk);
            next = 0;
            if (valid)
                $display("Random[%0d] = %016h", i, rand_out);
            #10;
        end
        $display("Test complete.");
        $finish;
    end

endmodule
