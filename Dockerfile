# Build the optimised static web UI, then serve it with Apache httpd.
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.11_10_1.12.15_3.8.4 AS build
WORKDIR /src
# Resolve dependencies in their own layer so source edits don't re-download them.
COPY project/build.properties project/plugins.sbt project/
COPY build.sbt .
RUN sbt -batch web/update
COPY core core
COPY web web
RUN sbt -batch web/webDist

FROM httpd:2.4.69-alpine
# Compress the HTML/JS (the Scala.js bundle compresses well).
RUN sed -i 's/^#LoadModule deflate_module/LoadModule deflate_module/' conf/httpd.conf \
 && echo 'AddOutputFilterByType DEFLATE text/html text/css text/javascript application/javascript' >> conf/httpd.conf
COPY --from=build /src/web/target/dist/ htdocs/
EXPOSE 80
