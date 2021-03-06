JARS = core.jar:jbox2d.jar
JAVAFILES = ProcessingDebugDraw.java Visualizer.java
SCALAFILES = Types.scala Physics.scala Main.scala Controller.scala StopSignController.scala StopLightController.scala SimpleReserver.scala AdvancedReserver.scala Wrappers.scala Lanes.scala

SCALAFLAGS = -deprecation

all : $(JAVAFILES)
	javac -cp $(JARS):. -d .  $(JAVAFILES)
	fsc -cp $(JARS):. $(SCALAFLAGS) $(SCALAFILES)

clean : 
	rm -f *.class