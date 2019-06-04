package com.facebook.buck_project_builder.parser;

import com.facebook.buck_project_builder.parser.generated.Python3BaseVisitor;
import com.facebook.buck_project_builder.parser.generated.Python3Parser;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class GeneratedParserWrapperTest {

  private static void expectSyntaxError(String code) {
    try {
      new GeneratedParserWrapper(code).getFileInputContext();
      fail("Provided code below should get syntax error, but we don't get one.\n" + code);
    } catch (SyntaxError error) {
      // expected
    }
  }

  @Test
  public void badPythonFilesDoGetSyntaxErrors() {
    // random characters
    expectSyntaxError("42haha\n");
    // indentation error
    expectSyntaxError("if True:\npass\n");
    // bad string literal
    expectSyntaxError("hello_world = \"hello world\n");
    // not balanced parentheses
    expectSyntaxError("print(\"hello world\"\n");
    // extra literal
    expectSyntaxError("a = 3 + 5 7\n");
    // bad function call
    expectSyntaxError("function_call(a, b='ha, c)\n");
  }

  /**
   * This test only ensures that the generated parser is correctly set up and we can write a simple
   * visitor for it. The visitor checks whether the simple AST conforms to the tree structure we
   * expect.
   */
  @Test
  public void canVisitSimpleFunctionDeclaration() throws SyntaxError {
    /*
     * Expected parse tree:
     * - root (file input context)
     *   - a list of statement context
     *      - [0]: compound statement
     *        - function definition
     *          - ...
     */
    GeneratedParserWrapper generatedParserWrapper = new GeneratedParserWrapper("def foo(): pass\n");
    Python3Parser.File_inputContext fileInputContext = generatedParserWrapper.getFileInputContext();

    List<Python3Parser.StmtContext> statementContextList = fileInputContext.stmt();
    assertEquals("should contain only one function declaration", 1, statementContextList.size());

    Python3Parser.StmtContext statementContext = statementContextList.get(0);
    assertNull("the code should contain no simple statements", statementContext.simple_stmt());

    assertNotNull(
        "the code should contain one compound statement", statementContext.compound_stmt());
    // it should only encounter function definition below, then it will return true since the
    // visitor for function definition returns true.
    assertTrue(statementContext.compound_stmt().accept(new SimpleFunctionDefinitionVisitor()));
  }

  private static final class SimpleFunctionDefinitionVisitor extends Python3BaseVisitor<Boolean> {
    @Override
    public Boolean visitFuncdef(Python3Parser.FuncdefContext functionDefinitionContext) {
      /*
       * Expected parse tree:
       * - function definition
       *   - name: foo
       *   - parameters: null (since it's empty)
       *   - suite:
       *     - simple statement:
       *       - small statement list:
       *         - [0]: pass statement
       */
      assertEquals(
          "function name should be foo",
          "foo",
          functionDefinitionContext.NAME().getSymbol().getText());

      assertNull("foo has no parameters", functionDefinitionContext.parameters().typedargslist());

      // we can locate the pass statement
      Python3Parser.SuiteContext suiteContext = functionDefinitionContext.suite();
      assertEquals(
          "no statement list should be in the function body", 0, suiteContext.stmt().size());
      Python3Parser.Simple_stmtContext simpleStatementContext = suiteContext.simple_stmt();
      assertNotNull(
          "there should be a simple pass statement",
          simpleStatementContext.small_stmt(0).pass_stmt());
      return true;
    }
  }
}
