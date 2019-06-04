package com.facebook.buck_project_builder.parser;

import com.facebook.buck_project_builder.parser.generated.Python3Lexer;
import com.facebook.buck_project_builder.parser.generated.Python3Parser;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import java.io.IOException;
import java.io.InputStream;

public final class GeneratedParserWrapper {

  private final Python3Parser generatedParser;
  private final SyntaxErrorListener syntaxErrorListener;

  private GeneratedParserWrapper(CharStream charInputStream) {
    Python3Lexer lexer = new Python3Lexer(charInputStream);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    generatedParser = new Python3Parser(tokens);
    syntaxErrorListener = new SyntaxErrorListener();
    generatedParser.removeErrorListeners();
    generatedParser.addErrorListener(syntaxErrorListener);
  }

  public GeneratedParserWrapper(String codeString) {
    this(CharStreams.fromString(codeString));
  }

  public GeneratedParserWrapper(InputStream inputStream) throws IOException {
    this(CharStreams.fromStream(inputStream));
  }

  /**
   * @return the root context (file input context). It can be used as the entry point of visitors.
   */
  public Python3Parser.File_inputContext getFileInputContext() throws SyntaxError {
    Python3Parser.File_inputContext rootContext = generatedParser.file_input();
    StringBuilder errorMessageBuilder = syntaxErrorListener.errorMessageBuilder;
    if (errorMessageBuilder.length() == 0) {
      return rootContext;
    }
    throw new SyntaxError(errorMessageBuilder.toString());
  }

  /**
   * The listener that collects all syntax errors during parsing the file and merge it into a single
   * string.
   */
  private static final class SyntaxErrorListener extends BaseErrorListener {

    private final StringBuilder errorMessageBuilder = new StringBuilder();

    @Override
    public void syntaxError(
        Recognizer<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException e) {
      errorMessageBuilder
          .append("error ")
          .append(line)
          .append(':')
          .append(charPositionInLine)
          .append(": ")
          .append(msg)
          .append('\n');
    }
  }
}
