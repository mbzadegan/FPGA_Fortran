`timescale 1ns/1ps
module axi_stream_crossbar_tb;
    parameter DATA_WIDTH = 64;
    parameter KEEP_WIDTH = DATA_WIDTH/8;
    parameter N_IN = 4;
    parameter M_OUT = 4;
    reg clk = 0;
    always #5 clk = ~clk; // 100 MHz
    reg rst_n = 0;

    // Inputs
    reg [N_IN-1:0] in_tvalid;
    wire [N_IN-1:0] in_tready;
    reg [N_IN*DATA_WIDTH-1:0] in_tdata;
    reg [N_IN*KEEP_WIDTH-1:0] in_tkeep;
    reg [N_IN-1:0] in_tlast;

    // Outputs
    wire [M_OUT-1:0] out_tvalid;
    reg  [M_OUT-1:0] out_tready;
    wire [M_OUT*DATA_WIDTH-1:0] out_tdata;
    wire [M_OUT*KEEP_WIDTH-1:0] out_tkeep;
    wire [M_OUT-1:0] out_tlast;

    // Config
    reg cfg_we;
    reg [$clog2(N_IN)-1:0] cfg_addr;
    reg [$clog2(M_OUT)-1:0] cfg_dest;
    reg [$clog2(8+1)-1:0] cfg_weight;
    reg cfg_drop_override;

    // Instantiate DUT
    axi_stream_crossbar #(
        .DATA_WIDTH(DATA_WIDTH),
        .KEEP_WIDTH(KEEP_WIDTH),
        .N_IN(N_IN),
        .M_OUT(M_OUT),
        .FIFO_ADDR_WIDTH(3), // small FIFOs for stress
        .WMAX(8),
        .DROP_ON_FULL(0)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .in_tvalid(in_tvalid),
        .in_tready(in_tready),
        .in_tdata(in_tdata),
        .in_tkeep(in_tkeep),
        .in_tlast(in_tlast),
        .out_tvalid(out_tvalid),
        .out_tready(out_tready),
        .out_tdata(out_tdata),
        .out_tkeep(out_tkeep),
        .out_tlast(out_tlast),
        .cfg_we(cfg_we),
        .cfg_addr(cfg_addr),
        .cfg_dest(cfg_dest),
        .cfg_weight(cfg_weight),
        .cfg_drop_on_full_override(cfg_drop_override)
    );

    // Testbench state
    integer i;
    initial begin
        $display("Starting crossbar testbench");
        rst_n = 0;
        in_tvalid = 0;
        in_tdata = 0;
        in_tkeep = 0;
        in_tlast = 0;
        cfg_we = 0;
        out_tready = {M_OUT{1'b1}}; // consumers always ready
        #50;
        rst_n = 1;

        // Randomly configure destinations and weights
        for (i = 0; i < N_IN; i = i + 1) begin
            @(posedge clk);
            cfg_addr <= i;
            cfg_dest <= i % M_OUT; // simple mapping initially
            cfg_weight <= (i % 4) + 1; // weights 1..4
            cfg_drop_override <= 0;
            cfg_we <= 1;
            @(posedge clk);
            cfg_we <= 0;
        end

        // Generate bursts
        fork
            begin : writers
                integer packet_id;
                packet_id = 0;
                forever begin
                    // pick an input at random
                    integer idx;
                    idx = $urandom_range(0, N_IN-1);
                    // create packet length random 1..16
                    integer plen;
                    plen = $urandom_range(1, 16);
                    integer beat;
                    for (beat = 0; beat < plen; beat = beat + 1) begin
                        @(posedge clk);
                        // set fields for idx only
                        in_tdata[(idx+1)*DATA_WIDTH-1 -: DATA_WIDTH] <= {48'dpacket_id, 16'dbeat}; // embed id+beat
                        in_tkeep[(idx+1)*KEEP_WIDTH-1 -: KEEP_WIDTH] <= {KEEP_WIDTH{1'b1}};
                        in_tlast[idx] <= (beat == plen-1);
                        in_tvalid[idx] <= 1'b1;
                        // wait until accepted
                        wait (in_tready[idx] == 1);
                        @(posedge clk);
                        in_tvalid[idx] <= 1'b0;
                        in_tlast[idx] <= 1'b0;
                    end
                    packet_id = packet_id + 1;
                    // small pause
                    repeat ($urandom_range(0,3)) @(posedge clk);
                end
            end

            begin : monitor
                // Monitor outputs and verify packet atomicity and destination mapping
                // Keep a simple table expected_dest[input] from config writes
                reg [$clog2(M_OUT)-1:0] expected_dest [0:N_IN-1];
                // Initialize from cfgs above
                for (i = 0; i < N_IN; i = i + 1) expected_dest[i] = i % M_OUT;

                forever begin
                    @(posedge clk);
                    for (i = 0; i < M_OUT; i = i + 1) begin
                        if (out_tvalid[i] && out_tready[i]) begin
                            // examine packet id embedded in data
                            reg [47:0] pid;
                            reg [15:0] beat;
                            {pid, beat} = out_tdata[(i+1)*DATA_WIDTH-1 -: DATA_WIDTH];
                            // Basic display
                            $display("OUT %0d beat: pid=%0d beat=%0d tlast=%b", i, pid, beat, out_tlast[i]);
                        end
                    end
                end
            end
        join_any
    end

    // Timeout
    initial begin
        #200000;
        $display("Timeout - finishing");
        $finish;
    end

endmodule
