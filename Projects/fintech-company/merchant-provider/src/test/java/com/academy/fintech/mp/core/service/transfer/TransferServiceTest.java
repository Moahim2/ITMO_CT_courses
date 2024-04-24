package com.academy.fintech.mp.core.service.transfer;

import com.academy.fintech.mp.public_interface.transfer.dto.CheckTransferDto;
import com.academy.fintech.mp.public_interface.transfer.dto.ExecuteTransferDto;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.math.BigDecimal;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;

import static com.academy.fintech.util.RandomUtil.genRandomBigDecimal;

@RunWith(SpringRunner.class)
@SpringBootTest
public class TransferServiceTest {
    private static final String PREFIX_OF_TRANSFER_ID = "tr";

    private final Set<String> idsTransfers = ConcurrentHashMap.newKeySet();

    @Autowired
    private TransferService transferService;

    @ParameterizedTest
    @CsvSource(value = {
            "0.1", "10", "100000",
            "0.213", "200", "992342924294",
            "0.8", "1", "10000.31241",
            "100", "1000", "10000"
    })
    public void testExecute(double amount) {
        simpleExecuteTest(new BigDecimal(amount));
    }

    @RepeatedTest(100)
    public void randomTestExecute() {
        simpleExecuteTest(genRandomBigDecimal(1, 1000000000000.0));
    }

    @RepeatedTest(10)
    public void randomTestCheck() throws ExecutionException, InterruptedException {
        String newTransferId = transferService.executeTransfer(createExecuteTransferDto(genRandomBigDecimal(1, 100000)));

        Assertions.assertTrue(idsTransfers.add(newTransferId));

        for (String transferId : idsTransfers) {
            CheckTransferDto answer = transferService.checkTransfer(createCheckTransferDto(transferId));
            if (answer.isCompleted()) {
                Assertions.assertNotNull(answer.date());
            } else {
                Assertions.assertNull(answer.date());
            }
        }
    }

    private void simpleExecuteTest(BigDecimal amount) {
        String transferId = transferService.executeTransfer(createExecuteTransferDto(amount));

        Assertions.assertTrue(transferId.startsWith(PREFIX_OF_TRANSFER_ID));
        Assertions.assertTrue(idsTransfers.add(transferId));
    }

    private static ExecuteTransferDto createExecuteTransferDto(BigDecimal amount) {
        return ExecuteTransferDto.builder()
                .amount(amount)
                .build();
    }

    private static CheckTransferDto createCheckTransferDto(String transferId) {
        return CheckTransferDto.builder()
                .transferId(transferId)
                .build();
    }
}
