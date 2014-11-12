/*
class MyOpenSoC(NumPorts: UInt) extends OpenSoC {
	val io = new Bundle {
		val Ports = Vec.fill(NumPorts) (new AXI())
	}
	val myInterfaces = Vec.fill(NumPorts) {Module (new NetworkInterface(NumPorts))}
	val myInjectionQs = Vec.fill(NumPorts) {Module (new InjectionQ())}
	val myTopo = Module(new Topology(NumPorts))
	val myEjectionQs = Vec.fill(NumPorts) {Module (new EjectionQ())}
	
	for (i <- 0 until NumPorts-1) {
		io.Ports(i) <> myInterfaces(i).io.AXIPort
		myInterfaces(i).io.OutChannels <> myInjectionQs(i).io.In
		myInjectionQs(i).io.Out <> myTopo.io.InChannels(i)
		myTopo.io.OutChannels(i) <> myEjectionQs(i).io.In
		myEjectionQs(i).io.Out <> myInterfaces(i).io.InChannels
	} 
}

class MyOpenSoC extends Module {
	val myCPUs = Vec.fill(5) {Module(new CPU())}
	val myHMC = Module(new HMC())
	val myTenGbE = Module(new TenGbE())
	val myPCIe = Module(new PCIe())
	val NumNodes = 8
	
	val OpenSoC = Module(new MyOpenSoC(NumNodes))
	
	for (i <- 0 until 4) {
		myCPUs(i).io.AXIPort <> MyOpenSoC.io.AXI(i)
	}
	myHMC.io.AXIPort <> MyOpenSoC.io.AXI(5)
	myTenGbE.io.AXIPort <> MyOpenSoC.io.AXI(6)
	myPCIe.io.AXIPort <> MyOpenSoC.io.AXI(7)
}
*/