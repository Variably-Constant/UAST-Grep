// SystemVerilog Test File for UAST-Grep
// Tests: modules, interfaces, classes, constraints, assertions

// =============================================================================
// Package Definition
// =============================================================================

package test_pkg;

  // Parameters
  parameter int MAX_ITEMS = 100;
  parameter string DEFAULT_NAME = "UAST-Grep";

  // Typedef
  typedef logic [31:0] word_t;
  typedef logic [7:0] byte_t;
  typedef logic [15:0] halfword_t;

  // Enum
  typedef enum logic [2:0] {
    IDLE    = 3'b000,
    READ    = 3'b001,
    WRITE   = 3'b010,
    EXECUTE = 3'b011,
    ERROR   = 3'b100
  } state_e;

  // Struct
  typedef struct packed {
    logic [31:0] addr;
    logic [31:0] data;
    logic [3:0]  be;
    logic        we;
  } bus_req_t;

  typedef struct packed {
    logic [31:0] data;
    logic        valid;
    logic        error;
  } bus_rsp_t;

  // Union
  typedef union packed {
    logic [31:0] word;
    logic [15:0] halfword[2];
    logic [7:0]  bytes[4];
  } data_u;

  // Function
  function automatic int calculate_parity(input logic [31:0] data);
    return ^data;
  endfunction

  // Task
  task automatic delay_cycles(input int cycles);
    repeat(cycles) @(posedge clk);
  endtask

endpackage

// =============================================================================
// Interface Definition
// =============================================================================

interface bus_if #(
  parameter int DATA_WIDTH = 32,
  parameter int ADDR_WIDTH = 32
) (
  input logic clk,
  input logic rst_n
);

  // Signals
  logic [ADDR_WIDTH-1:0] addr;
  logic [DATA_WIDTH-1:0] wdata;
  logic [DATA_WIDTH-1:0] rdata;
  logic [(DATA_WIDTH/8)-1:0] be;
  logic we;
  logic re;
  logic valid;
  logic ready;
  logic error;

  // Modports
  modport master (
    output addr, wdata, be, we, re, valid,
    input  rdata, ready, error,
    import master_write, master_read
  );

  modport slave (
    input  addr, wdata, be, we, re, valid,
    output rdata, ready, error,
    import slave_respond
  );

  modport monitor (
    input addr, wdata, rdata, be, we, re, valid, ready, error
  );

  // Interface methods
  task automatic master_write(
    input logic [ADDR_WIDTH-1:0] a,
    input logic [DATA_WIDTH-1:0] d
  );
    @(posedge clk);
    addr  <= a;
    wdata <= d;
    we    <= 1'b1;
    valid <= 1'b1;
    @(posedge clk);
    while (!ready) @(posedge clk);
    valid <= 1'b0;
    we    <= 1'b0;
  endtask

  task automatic master_read(
    input  logic [ADDR_WIDTH-1:0] a,
    output logic [DATA_WIDTH-1:0] d
  );
    @(posedge clk);
    addr  <= a;
    re    <= 1'b1;
    valid <= 1'b1;
    @(posedge clk);
    while (!ready) @(posedge clk);
    d = rdata;
    valid <= 1'b0;
    re    <= 1'b0;
  endtask

  task automatic slave_respond(
    input logic [DATA_WIDTH-1:0] d,
    input logic err = 1'b0
  );
    @(posedge clk);
    rdata <= d;
    error <= err;
    ready <= 1'b1;
    @(posedge clk);
    ready <= 1'b0;
  endtask

  // Assertions
  property valid_handshake;
    @(posedge clk) disable iff (!rst_n)
    valid |-> ##[1:10] ready;
  endproperty

  assert property (valid_handshake)
    else $error("Handshake timeout!");

  cover property (valid_handshake);

endinterface

// =============================================================================
// Module Definition
// =============================================================================

module processor #(
  parameter int DATA_WIDTH = 32,
  parameter int ADDR_WIDTH = 32,
  parameter int REG_COUNT  = 32
) (
  input  logic                  clk,
  input  logic                  rst_n,
  bus_if.master                 bus,
  output logic                  irq,
  output logic [DATA_WIDTH-1:0] debug_out
);

  import test_pkg::*;

  // Local parameters
  localparam int LOG_REG_COUNT = $clog2(REG_COUNT);

  // Signals
  state_e current_state, next_state;
  word_t  registers[REG_COUNT];
  word_t  pc;
  word_t  instruction;
  logic   stall;
  logic   flush;

  // Generate block
  genvar i;
  generate
    for (i = 0; i < REG_COUNT; i++) begin : reg_init
      always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
          registers[i] <= '0;
        end
      end
    end
  endgenerate

  // Sequential logic
  always_ff @(posedge clk or negedge rst_n) begin : state_reg
    if (!rst_n) begin
      current_state <= IDLE;
      pc <= '0;
    end else begin
      current_state <= next_state;
      if (!stall && !flush) begin
        pc <= pc + 4;
      end
    end
  end

  // Combinational logic
  always_comb begin : next_state_logic
    next_state = current_state;
    stall = 1'b0;

    unique case (current_state)
      IDLE: begin
        if (bus.valid) begin
          next_state = bus.we ? WRITE : READ;
        end
      end

      READ: begin
        if (bus.ready) begin
          next_state = EXECUTE;
        end else begin
          stall = 1'b1;
        end
      end

      WRITE: begin
        if (bus.ready) begin
          next_state = IDLE;
        end else begin
          stall = 1'b1;
        end
      end

      EXECUTE: begin
        next_state = IDLE;
      end

      ERROR: begin
        next_state = IDLE;
      end

      default: begin
        next_state = IDLE;
      end
    endcase
  end

  // Continuous assignment
  assign debug_out = pc;
  assign irq = (current_state == ERROR);

  // Function call
  logic parity;
  assign parity = calculate_parity(instruction);

  // Assertions
  assert property (@(posedge clk) disable iff (!rst_n)
    (current_state == ERROR) |-> ##1 (current_state == IDLE)
  ) else $error("Error state not recovered!");

  // Coverage
  covergroup state_cg @(posedge clk);
    state_cp: coverpoint current_state {
      bins idle    = {IDLE};
      bins read    = {READ};
      bins write   = {WRITE};
      bins execute = {EXECUTE};
      bins error   = {ERROR};
      illegal_bins illegal = default;
    }
  endgroup

  state_cg state_cov = new();

endmodule

// =============================================================================
// Class Definition (UVM-style)
// =============================================================================

class Transaction;
  rand logic [31:0] addr;
  rand logic [31:0] data;
  rand logic        write;
  rand int unsigned delay;

  // Constraints
  constraint addr_c {
    addr inside {[32'h0000_0000:32'h0000_FFFF]};
  }

  constraint data_c {
    if (write) {
      data != 0;
    }
  }

  constraint delay_c {
    delay inside {[1:10]};
  }

  // Constructor
  function new(string name = "Transaction");
    // Initialize
  endfunction

  // Methods
  virtual function void display();
    $display("Addr: 0x%08h, Data: 0x%08h, Write: %b", addr, data, write);
  endfunction

  virtual function Transaction copy();
    Transaction t = new();
    t.addr  = this.addr;
    t.data  = this.data;
    t.write = this.write;
    t.delay = this.delay;
    return t;
  endfunction

  function bit compare(Transaction other);
    return (this.addr == other.addr) &&
           (this.data == other.data) &&
           (this.write == other.write);
  endfunction

endclass

// Extended class
class BurstTransaction extends Transaction;
  rand int unsigned burst_length;
  rand logic [31:0] burst_data[];

  constraint burst_c {
    burst_length inside {[1:16]};
    burst_data.size() == burst_length;
  }

  function new(string name = "BurstTransaction");
    super.new(name);
  endfunction

  virtual function void display();
    super.display();
    $display("Burst Length: %0d", burst_length);
    foreach (burst_data[i]) begin
      $display("  [%0d]: 0x%08h", i, burst_data[i]);
    end
  endfunction

endclass

// =============================================================================
// Testbench
// =============================================================================

module tb_processor;

  // Clock and reset
  logic clk = 0;
  logic rst_n = 0;

  // Clock generation
  always #5 clk = ~clk;

  // Interface instance
  bus_if #(.DATA_WIDTH(32), .ADDR_WIDTH(32)) bus_inst(clk, rst_n);

  // DUT instance
  processor #(
    .DATA_WIDTH(32),
    .ADDR_WIDTH(32),
    .REG_COUNT(32)
  ) dut (
    .clk(clk),
    .rst_n(rst_n),
    .bus(bus_inst.master),
    .irq(),
    .debug_out()
  );

  // Test
  initial begin
    Transaction trans;

    // Reset
    rst_n = 0;
    repeat(10) @(posedge clk);
    rst_n = 1;

    // Create and randomize transaction
    trans = new("test_trans");
    assert(trans.randomize()) else $fatal("Randomization failed!");
    trans.display();

    // Run test
    repeat(100) @(posedge clk);

    $display("Test completed!");
    $finish;
  end

  // Timeout
  initial begin
    #10000;
    $fatal("Timeout!");
  end

endmodule
