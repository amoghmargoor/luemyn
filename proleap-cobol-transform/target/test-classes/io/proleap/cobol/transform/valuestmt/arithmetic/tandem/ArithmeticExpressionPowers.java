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
public class ArithmeticExpressionPowers {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: ARITHEXPR                                                                          //   (2)  PROGRAM-ID. ARITHEXPR.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=20, fraction=0)                                                                  //   (5)     01 SOMEID1 PIC 9(20).
    protected BigDecimal someid1;                                                                   
    public void procedureDivision() throws Exception{                                                //   (6)  PROCEDURE DIVISION.
        someid1 = BigDecimal.valueOf(16);                                                            //   (7)     COMPUTE SOMEID1 = 4 ** 2.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ArithmeticExpressionPowers.class);
        final ArithmeticExpressionPowers arithmeticexpressionpowers = context.getBean(ArithmeticExpressionPowers.class);
        arithmeticexpressionpowers.procedureDivision();
    }
}
