package cljd.lang;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

public class LineNumberingWriter extends FilterWriter {

  private int line = 1;
  private int column = 0;

    public LineNumberingWriter(Writer out) {
        super(out);
    }

    @Override
    public void write(int c) throws IOException {
        super.write(c);
        if (c == '\n') {
            line++;
            column = 0;
        } else column++;
    }

    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
        super.write(cbuf, off, len);
        for (int i = off; i < off + len; i++) {
            if (cbuf[i] == '\n') {
                line++;
                column = 0;
            } else column++;
        }
    }

    @Override
    public void write(String str, int off, int len) throws IOException {
        super.write(str, off, len);
        for (int i = off; i < off + len; i++) {
            if (str.charAt(i) == '\n') {
                line++;
                column = 0;
            } else column++;
        }
    }

    public int getLine() {
        return line;
    }

    public int getColumn() {
        return column;
    }
}
