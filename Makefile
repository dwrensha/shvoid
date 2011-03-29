JARS = core.jar:jbox2d.jar
JAVAFILES = ProcessingDebugDraw.java Visualizer.java
SCALAFILES = Physics.scala

all : $(JAVAFILES)
	javac -cp $(JARS) $(JAVAFILES)
	fsc -cp $(JARS):. $(SCALAFILES)