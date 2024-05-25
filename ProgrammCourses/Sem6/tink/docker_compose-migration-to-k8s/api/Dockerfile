FROM --platform=linux/amd64 openjdk:21-jdk-slim

COPY app-server/build/libs/app.jar app.jar

EXPOSE 9091
EXPOSE 8085

CMD ["java", "-jar", "app.jar"]