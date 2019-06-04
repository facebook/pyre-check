package com.facebook.buck_project_builder.parser;

import com.facebook.buck_project_builder.parser.generated.Python3Lexer;
import com.facebook.buck_project_builder.parser.generated.Python3Parser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

public final class GeneratedParserWrapper {

  private final Python3Parser generatedParser;

  private GeneratedParserWrapper(CharStream charInputStream) {
    Python3Lexer lexer = new Python3Lexer(charInputStream);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    generatedParser = new Python3Parser(tokens);
  }

  public GeneratedParserWrapper(String codeString) {
    this(CharStreams.fromString(codeString));
  }

  /**
   * @return the root context (file input context). It can be used as the entry point of visitors.
   */
  public Python3Parser.File_inputContext getFileInputContext() {
    return generatedParser.file_input();
  }
}
