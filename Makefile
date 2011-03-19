JARS = core.jar:jbox2d.jar
JAVAFILES = ProcessingDebugDraw.java Visualizer.java


all : $(JAVAFILES)
	javac -cp $(JARS) $(JAVAFILES)