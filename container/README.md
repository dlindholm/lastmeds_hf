# Container for the _lastmeds_ analyses

Docker is a useful way of containerizing an analysis environment, and works well
when run on a local machine when you have _root_ access. This prerequisite is 
obviously not met in an HPC environment. However, for such applications,
_Apptainer_ is a superb alternative, which can build containers from Docker
images.

The following steps are taken to build the .sif file:

1. Build the image: `docker buildx build --platform linux/amd64 - < Dockerfile -t dlindholm/lastmeds` (explicitly stating to build for amd64)
2. Login to Docker Hub: `docker login -u "<USERNAME>" docker.io`
3. Push to Docker Hub: `docker push <USERNAME>/lastmeds`
4. Connect to [Sylabs cloud](https://cloud.sylabs.io/)
5. Select _Remote builder_ and paste the info from `lastmeds.def`
6. Build
7. Download the .sif file
8. Upload to HPC environment (using SFTP)
9. Test containter for interactive use: `apptainer shell -C --mount src=.,dst=/lastmeds dlindholm_lastmeds_latest.sif` (the filesystem will be fully contained [-C] and the working directory will be mapped to the `/lastmeds` directory within the container)

NB: Above steps require (free) user accounts at Docker and Sylabs.