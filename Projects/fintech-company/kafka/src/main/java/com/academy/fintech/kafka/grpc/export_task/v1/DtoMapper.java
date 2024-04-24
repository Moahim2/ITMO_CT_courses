package com.academy.fintech.kafka.grpc.export_task.v1;

import com.academy.fintech.kafka.ExportNewTaskRequest;
import com.academy.fintech.kafka.grpc.export_task.v1.dto.NewExportTaskDto;
import org.springframework.stereotype.Component;

@Component
public class DtoMapper {

    public NewExportTaskDto mapRequestToDto(ExportNewTaskRequest request) {
        return NewExportTaskDto.builder()
                .type(request.getType())
                .stringKey(request.getStringKey())
                .numberKey(request.getNumberKey())
                .build();
    }

}
