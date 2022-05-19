FROM google/dart:2.15 AS service

WORKDIR /tmp

RUN git clone https://github.com/dart-lang/sdk.git && cd sdk && git checkout stable && cd ../

# generate vmservice java fiels
RUN cd sdk/pkg/vm_service && dart pub get && cd tool && dart generate.dart

FROM clojure AS clj

WORKDIR /tmp

RUN mkdir deps && cd deps && \
    echo "{:deps {com.google.code.gson/gson {:mvn/version \"2.2.4\"} com.google.guava/guava {:mvn/version \"13.0.1\"} com.github.pmairif/weberknecht {:mvn/version \"1.0.0\"}}}" > deps.edn

RUN cd deps && clojure -X:deps prep

# build vmservice.jar
FROM openjdk:7 AS builder

ENV ANT_VERSION=1.9.9
ENV ANT_HOME=/opt/ant
ENV DART_SDK=/usr/lib/dart

# change to tmp folder
WORKDIR /tmp

# Download and extract apache ant to opt folder
RUN wget --no-check-certificate --no-cookies http://archive.apache.org/dist/ant/binaries/apache-ant-${ANT_VERSION}-bin.tar.gz \
    && wget --no-check-certificate --no-cookies http://archive.apache.org/dist/ant/binaries/apache-ant-${ANT_VERSION}-bin.tar.gz.sha512 \
    && echo "$(cat apache-ant-${ANT_VERSION}-bin.tar.gz.sha512) apache-ant-${ANT_VERSION}-bin.tar.gz" | sha512sum -c \
    && tar -zvxf apache-ant-${ANT_VERSION}-bin.tar.gz -C /opt/ \
    && ln -s /opt/apache-ant-${ANT_VERSION} /opt/ant \
    && rm -f apache-ant-${ANT_VERSION}-bin.tar.gz \
    && rm -f apache-ant-${ANT_VERSION}-bin.tar.gz.sha512

# add executables to path
RUN update-alternatives --install "/usr/bin/ant" "ant" "/opt/ant/bin/ant" 1 && \
    update-alternatives --set "ant" "/opt/ant/bin/ant"

WORKDIR /vmservice/

COPY --from=service /tmp/sdk/pkg/vm_service vm_service

RUN git clone https://github.com/flutter/flutter-intellij.git && cd flutter-intellij && git checkout a9c8b2d9126d77ca9c6363d3fd390cc87a19bcec

RUN mkdir -p vm_service/java/third_party/guava && mkdir vm_service/java/third_party/gson && mkdir vm_service/java/third_party/weberknecht
COPY --from=clj  /root/.m2/repository/com/google/guava/guava/13.0.1/guava-13.0.1.jar vm_service/java/third_party/guava/guava-13.0.1.jar
COPY --from=clj  /root/.m2/repository/com/google/code/gson/gson/2.2.4/gson-2.2.4.jar vm_service/java/third_party/gson/gson-2.2.4.jar
RUN  cp /vmservice/flutter-intellij/third_party/lib/dart-plugin/deps/weberknecht-0.1.5.jar vm_service/java/third_party/weberknecht/weberknecht-0.1.5.jar

RUN rm vm_service/java/build.xml

WORKDIR /vmservice/vm_service/java/

RUN echo '<project name="vm_service_lib" default="dist">\
  <target name="clean">\n\
    <delete dir="build"/>\n\
  </target>\n\
\n\
  <target name="init">\n\
    <mkdir dir="build"/>\n\
    <mkdir dir="dist"/>\n\
\n\
    <property environment="env"/>\n\
    <property file="version.properties" prefix="service"/>\n\
    <property name="build.id" value=""/>\n\
\n\
    <property\n\
        name="path"\n\
        value="third_party/gson/gson-2.2.4.jar;third_party/guava/guava-13.0.1.jar;third_party/weberknecht/weberknecht-0.1.5.jar"/>\n\
  </target>\n\
\n\
  <target name="compile" depends="init">\n\
    <!-- compile library source -->\n\
    <mkdir dir="build/classes"/>\n\
    <javac srcdir="src" destdir="build/classes" includeantruntime="false"\n\
        source="1.7" target="1.7"\n\
        classpath="${path}" debug="true"/>\n\
\n\
    </target>\n\
\n\
  <target name="jar" depends="compile">\n\
    <copy file="version.properties" todir="build/classes"/>\n\
    <jar destfile="build/vm_service_lib.jar" basedir="build/classes"/>\n\
    <jar destfile="build/vm_service_lib-src.jar" basedir="src"/>\n\
  </target>\n\
\n\
  <target name="test" depends="compile">\n\
    <java classname="org.dartlang.vm.service.VmServiceTest" fork="true" failonerror="true">\n\
      <arg value="${env.DART_SDK}"/>\n\
      <classpath>\n\
        <pathelement path="${path}"/>\n\
        <pathelement location="build/classes"/>\n\
        <pathelement location="build/test"/>\n\
      </classpath>\n\
    </java>\n\
  </target>\n\
\n\
  <target name="dist" depends="jar,test">\n\
    <!-- copy and rename the library -->\n\
    <copy file="build/vm_service_lib.jar" tofile="dist/vm_service_lib-${service.version}${build.id}.jar"/>\n\
    <copy file="build/vm_service_lib-src.jar" tofile="dist/vm_service_lib-${service.version}${build.id}-src.jar"/>\n\
  </target>\n\
</project>\
' > build.xml

RUN ant jar

FROM busybox AS package

WORKDIR /dist

COPY --from=builder /vmservice/vm_service/java/third_party/weberknecht/weberknecht-0.1.5.jar weberknecht-0.1.5.jar
COPY --from=builder /vmservice/vm_service/java/third_party/guava/guava-13.0.1.jar guava-13.0.1.jar
COPY --from=builder /vmservice/vm_service/java/third_party/gson/gson-2.2.4.jar gson-2.2.4.jar
COPY --from=builder /vmservice/vm_service/java/build/vm_service_lib.jar vm_service_lib.jar
