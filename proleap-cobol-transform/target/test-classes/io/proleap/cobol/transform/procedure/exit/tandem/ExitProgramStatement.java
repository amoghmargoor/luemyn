import java.lang.String;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import javax.validation.constraints.*;
import io.proleap.cobol.api.*;
import io.proleap.cobol.api.data.*;
import io.proleap.cobol.api.environment.configuration.object.*;
import io.proleap.cobol.api.environment.configuration.source.*;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.*;
import io.proleap.cobol.api.ProLeapCobolApiSpringConfig;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

@Singleton
public class ExitProgramStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: EXITSTMT                                                                           //   (2)  PROGRAM-ID. EXITSTMT.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        System.exit(0);                                                                              //   (4)     EXIT PROGRAM.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ExitProgramStatement.class);
        final ExitProgramStatement exitprogramstatement = context.getBean(ExitProgramStatement.class);
        exitprogramstatement.procedureDivision();
    }
}
