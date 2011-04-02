JARS = core.jar:jbox2d.jar
JAVAFILES = ProcessingDebugDraw.java Visualizer.java
SCALAFILES = Types.scala Physics.scala Main.scala Controller.scala

all : $(JAVAFILES)
	javac -cp $(JARS) $(JAVAFILES)
	fsc -cp $(JARS):. $(SCALAFILES)