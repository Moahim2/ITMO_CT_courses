plugins {
    kotlin("jvm") version "1.9.10"
    java
}

    tasks {
        test {
            maxHeapSize = "4g"
            jvmArgs(
                "--add-opens", "java.base/jdk.internal.misc=ALL-UNNAMED",
                "--add-exports", "java.base/jdk.internal.util=ALL-UNNAMED",
                "--add-exports", "java.base/sun.security.action=ALL-UNNAMED"
            )
        }
    }

group = "ru.itmo.mpp"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    implementation(kotlin("reflect"))
    testImplementation(kotlin("test-junit"))
    testImplementation("org.jetbrains.kotlinx:lincheck:2.23")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

sourceSets.main {
    java.srcDir("src")
}

sourceSets.test {
    java.srcDir("test")
}