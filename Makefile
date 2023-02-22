TARGET=parsetype
$(TARGET): main.c
	gcc -o $(TARGET) main.c

run: $(TARGET)
	./$(TARGET)

debug: main.c clean
	gcc -g -O0 -o $(TARGET) main.c

clean:
	$(RM) $(TARGET)
