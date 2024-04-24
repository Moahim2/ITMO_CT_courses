package com.academy.fintech.mp.rest.transfer.mock;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.testcontainers.shaded.com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static com.academy.fintech.util.RandomUtil.genRandomBigDecimal;

/**
 * Mock-tests for mp end-points.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@AutoConfigureMockMvc
public class TransferControllerTest {
    private static final String EXECUTE_URL = "/mp/transfer/execute";

    @Autowired
    private MockMvc mockMvc;

    private final ObjectMapper objectMapper = new ObjectMapper();
    private final Set<String> idsTransfers = ConcurrentHashMap.newKeySet();

    @SuppressWarnings("unchecked")
    @RepeatedTest(100)
    public void testExecuteTransferEndPoint() throws Exception {
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post(EXECUTE_URL)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(genJSONExecuteTransferRequest()))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
        Map<String, String> response = (Map<String, String>) objectMapper.readValue(
                mvcResult.getResponse().getContentAsString(),
                Map.class
        );
        Assertions.assertEquals(1, response.size());
        Assertions.assertTrue(idsTransfers.add(response.get("transferId")));
    }

    private static String genJSONExecuteTransferRequest() {
        return """
                {
                    "amount": "%s"
                }""".formatted(genRandomBigDecimal(1, 100000).toString());
    }
}
