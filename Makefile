TARGET=parsetype
$(TARGET): main.c
	gcc -o $(TARGET) main.c

run: $(TARGET)
	./$(TARGET)

clean:
	$(RM) $(TARGET)
