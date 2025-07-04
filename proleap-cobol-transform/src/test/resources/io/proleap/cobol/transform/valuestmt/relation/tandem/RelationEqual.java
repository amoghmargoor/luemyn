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
public class RelationEqual {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: COND                                                                               //   (2)  PROGRAM-ID. COND.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        if (true) {                                                                                  //   (4)     IF 1 EQUAL 1 THEN END-IF.
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, RelationEqual.class);
        final RelationEqual relationequal = context.getBean(RelationEqual.class);
        relationequal.procedureDivision();
    }
}
