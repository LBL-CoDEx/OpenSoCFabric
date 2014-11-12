package OpenSoC

import Chisel._
/*
class AXI4Lite32 extends Bundle {
	// AXI Global Signals
	val ACLK =		Bool(INPUT)
	val ARESETn =	Bool(INPUT)
	
	// AXI Write Address Channel Signals
	val AWVALID =	Bool(INPUT)
	val AWREADY =	Bool(OUTPUT)
	val AWADDR =	UInt(INPUT, 32)
	val AWPROT =	UInt(INPUT, 3)
	
	// AXI Write Data Channel Signals
	val WVALID =	Bool(INPUT)
	val WREADY =	Bool(OUTPUT)
	val WDATA =		UInt(INPUT, 32)
	val WSTRB =		Bool(INPUT)
	
	// AXI Write Response Channel Signals
	val BVALID =	Bool(INPUT)
	val BREADY =	Bool(INPUT)
	val BRESP =		Bool(OUTPUT)
	
	// AXI Read Address Channel Signals
	val ARVALID =	Bool(INPUT)
	val ARREADY =	Bool(OUTPUT)
	val ARADDR =	UInt(INPUT, 32)
	val ARPROT =	UInt(INPUT, 3)
	
	// AXI Read Data Channel Signals
	val RVALID =	Bool(OUTPUT)
	val RREADY =	Bool(INPUT)
	val RDATA =		UInt(OUTPUT, 32)
	val RRESP =		Bool(OUTPUT)
}

abstract class AXI extends Bundle {
	// AXI Global Signals
	val ACLK =		Bool(INPUT)
	val ARESETn =	Bool(INPUT)
	
	// AXI Write Address Channel Signals
	val AWID	i
	val AWADDR	i
	val AWLEN	i
	val AWSIZE	i
	val AWBURST	i
	val AWLOCK	i
	val AWCACHE	i
	val AWPROT =	UInt(INPUT, 3)
	val AWCACHE =	UInt(INPUT, 4)
	val AWQOS	i
	val AWREGION	i
	val AWUSER	i
	val AWVALID	i
	val AWREADY	o
	
	// AXI Write Data Channel Signals
	val WID		i
	val WDATA	i
	val WSTRB	i
	val WLAST	i
	val WUSER	i
	val WVALID	i
	val WREADY	o
	
	// AXI Write Response Channel Signals
	val BID	o
	val BRESP	o
	val BUSER	o
	val BVALID	o
	val BREADY	i
	
	// AXI Read Address Channel Signals
	val ARID	i
	val ARADDR	i
	val ARLEN	i
	val ARSIZE	i
	val ARBURST	i
	val ARLOCK	i
	val ARCACHE =	UInt(INPUT, 4)
	val ARPROT =	UInt(INPUT, 3)
	val ARQOS	i
	val ARREGION	i
	val ARUSER	i
	val ARVALID	i
	val ARREADY	o
	
	// AXI Read Data Channel Signals
	val RID	o
	val RDATA	o
	val RRESP	o
	val RLAST	o
	val RUSER	o
	val RVALID	o
	val RREADY	i
	
	// AXI Low-Power Interface Signals
	val CSYSREQ	i
	val CSYSACK	o
	val CACTIVE	o
}
*/
